package com.github.kright

import java.awt.image.BufferedImage
import java.util.concurrent.Executors

import com.github.kright.Color

import scala.annotation.tailrec
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._


class RayTracer(val models: Seq[Model], val eps: Double = 0.001, val maxDist: Double = 1000.0, val maxBounces: Int = 10):
  @tailrec
  final def trace(ray: Ray, f: SDF, currentT: Double, maxT: Double): Option[Double] =
    if (currentT >= maxT)
      None
    else
      val dist = f(ray.shifted(currentT))
      if (dist < eps)
        Option(currentT)
      else
        trace(ray, f, currentT + dist, maxT)

  def trace[T <: Model](ray: Ray, models: Seq[T]): Option[(Double, T)] =
    models.foldLeft(Option[(Double, T)](null)) {
      (prev, newModel) =>
        val threshold = prev.map((minT, _) => minT).getOrElse(maxDist)
        trace(ray, newModel.sdf, 0, threshold) match
          case Some(betterT) => Option(betterT, newModel)
          case None => prev
    }

  def render(ray: Ray, bouncesCount: Int): Option[Color] = 
    if bouncesCount <= 0
      None
    else  
      trace(ray, models).map{ (t, model) =>
        val pos = ray.shifted(t)
        model.material.getColor(pos, ray.direction, model.sdf.getNormal(pos), this, bouncesCount - 1)
      }
  

  def renderMultithread(raysGenerator: RaysGenerator, screenSize: ScreenSize, threadsCount: Int): BufferedImage = 
    implicit val screenSize2 = screenSize

    val pool = Executors.newFixedThreadPool(threadsCount)
    implicit val executors = ExecutionContext.fromExecutor(pool)
    
    val futures =  
      for
        y <- 0 until screenSize.height;
        x <- 0 until screenSize.width 
      yield 
        Future {
          val colors: Seq[Color] = raysGenerator.createRays(x, y).map(ray => render(ray, maxBounces).getOrElse(Color.black))
          val meanColor = getMean(colors)
          val mapped = meanColor / (meanColor + Color(1, 1, 1))
          (x, y, ((mapped ^ 0.45) * 255).toRGBInt)
        }

    val image = new BufferedImage(screenSize.width, screenSize.height, BufferedImage.TYPE_INT_RGB)
    futures.foreach { f =>
      val (x, y, pixelColor) = Await.result(f, 60.seconds);
      image.setRGB(x, y, pixelColor)
    }
    
    pool.shutdown()
    
    image

  def render(raysGenerator: RaysGenerator, screenSize: ScreenSize): BufferedImage =
    implicit val screenSize2 = screenSize
  
    val image = new BufferedImage(screenSize.width, screenSize.height, BufferedImage.TYPE_INT_RGB)
    
    for
      y <- 0 until screenSize.height
      x <- 0 until screenSize.width 
    do
      val colors: Seq[Color] = raysGenerator.createRays(x, y).map(ray => render(ray, maxBounces).getOrElse(Color.black))
      val meanColor = getMean(colors)
      val mapped = meanColor / (meanColor + Color(1, 1, 1))
      image.setRGB(x, y, ((mapped ^ 0.45) * 255).toRGBInt)
  
    image
    