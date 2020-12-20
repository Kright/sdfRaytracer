package com.github.kright

import java.awt.image.BufferedImage
import java.io.File

import com.github.kright
import com.github.kright.Color
import javax.imageio.ImageIO

import scala.annotation.tailrec
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val scene = Seq(
      Model(Sphere(Vector(z = 1.0), radius = 0.1), Diffuse(Color(1, 1, 1))),
      Model(Sphere(Vector(x = -0.5, y = 0.05, z = 1.0), radius = 0.3), FresnelShlick(0.04, Mirror(Color(1.0, 1.0, 1.0)), Checkers(0.08, Glossy(Color(0.9, 0.9, 0.9), roughness = 0.2), Glossy(Color(0.4, 0.4, 0.4), roughness = 0.01)))),
      Model(Sphere(Vector(x = 0.1 / 1.44, y = 0.1 / 1.44, z = 1.0), radius = 0.05), FresnelShlick(0.04, Mirror(Color(1.0, 1.0, 1.0)), Diffuse(Color(0.9, 0.9, 0.9)))),
      Model(Sphere(Vector(x = -0.1 / 1.44, y = 0.1 / 1.44, z = 1.0), radius = 0.05), Diffuse(Color(1, 0.5, 1))),
      Model(Ellipsis(Vector(0.1, -0.2, 1.0), Vector(-0.1, -0.2, 0.8), radius = 0.3), Mirror(Color(0.5, 0.9, 0.5))),
      Model(Combination(Seq[(SDF, Double)](Cube(Vector(0.3, 0.0, 1.0), 0.1) -> 0.8, Sphere(Vector(0.3, 0.0, 1.0), 0.1) -> 0.2)), Diffuse(Color(0.9, 0.05, 0.9))),
      Model(Plane(Vector(y = -2), Vector(y = 1)), Diffuse(Color(0.2, 0.9, 0.2))),  // ground
      Model(Plane(Vector(y = 100), Vector(y = -1)), EmissiveColor(Color(0.8, 0.9, 1.0))), // sky
    )

    val cubes = 
      for 
        i <- -1 to 1; 
        j <- -1 to 5; 
        pos = Vector(0.1 + i * 0.3, -0.3, 1.0 + j * 0.3)
        cube = Cube(pos, 0.1)
        sphere = Sphere(pos, 0.1)
        w = MyRandom.nextDouble() 
        material = Glossy(MyRandom.nextRandomColor(), roughness = MyRandom.nextDouble()) 
      yield 
        if (w > 0.4)
          Model(Combination(Seq(cube -> 1.0, sphere -> w)), material)
        else 
          Model(cube, material)

    val skyAndGround = {
      val groundLevel = -2.0
      val skyLevel = 100.0
      val distance = 500.0
      val sunSize = 20.0
      val skyColor =  EmissiveColor(Color(0.8, 0.9, 2.0) * 3 )
      Seq(
        Model(Plane(Vector(y = groundLevel), Vector(y = 1)), Diffuse(Color(0.1, 0.8, 0.1))),  // ground
        Model(Plane(Vector(y = skyLevel), Vector(y = -1)),skyColor),
  
        Model(Plane(Vector(x = distance), Vector(x = -1)), skyColor), // sky
        Model(Plane(Vector(x = -distance), Vector(x = 1)), skyColor), // sky
        Model(Plane(Vector(z = distance), Vector(z = -1)), skyColor), // sky
        Model(Plane(Vector(z = -distance), Vector(z = 1)), skyColor), // sky

        Model(Sphere(Vector(x = skyLevel - 2 * sunSize, y = skyLevel - 2 * sunSize), radius = sunSize), EmissiveColor(Color(11, 10, 5) * 2)) // sun
      )
    }
    
    val reftaction: Refraction = Refraction(1.5)
    val refractionInverted: Refraction = reftaction.inverted
    
    val lens = Model(Sphere(Vector(0, 0, 1), 0.2), FresnelShlick(0.04, Mirror(Color(0.95, 0.95, 0.95)), reftaction))
    val innerLens = Model(InvertedSDF(lens.sdf), FresnelShlick(0.04, Mirror(Color(0.95, 0.95, 0.95)), refractionInverted))
    
//    val models = cubes ++ skyAndGround ++ scene
    val models = cubes ++ skyAndGround :+ lens 
    
    val div = 2
    val screenSize = ScreenSize(800 / div, 600 / div)
    val camera = Camera(1.0, screenSize.height.toDouble / screenSize.width.toDouble)
    val msaaGenerator = MSAAGenerator(camera, MSAAGenerator.random(128 / div))
//    val dofGenerator = DOFGenerator(camera, focalLength = 1.4, lensSize = 0.03, MyRandom.nextPairInCircle, msaaGenerator)
    
    println("start!")
    val start = System.nanoTime()
    val raytracer = new RayTracer(models, maxBounces = 16)
    val innerRaytracer = new RayTracer(Seq(innerLens), maxBounces = 16)
    
    reftaction.innerRaytracer = innerRaytracer
    refractionInverted.innerRaytracer = raytracer
    
    val pic = raytracer.renderMultithread(msaaGenerator, screenSize, threadsCount = 4)
    
    val finish = System.nanoTime()
    println(s"finished, dt = ${(finish - start).toString.reverse.grouped(3).mkString("_").reverse}ns")

    val imgName = s"render${screenSize.width}x${screenSize.height}.png"
    ImageIO.write(pic, "png", new File(imgName))
    println(s"saved as $imgName")
  }
}

def getMean(colors: Seq[Color]): Color =
  var r, g, b = 0.0
  colors.foreach { c => r += c.r; g += c.g; b += c.b }
  Color(r, g, b) / colors.size.toDouble

case class Ray private(start: Vector, direction: NormalizedVector):
  def shifted(t: Double) = start + direction * t

object Ray:
  def apply(start: Vector, direction: Vector): Ray = new Ray(start, direction.normalized)
  def fromTo(from: Vector, to: Vector): Ray = Ray(from, to - from)

case class Camera(pos: Vector, right: Vector, up: Vector, forward: Vector)

object Camera:
  def apply(fovX: Double, fovY: Double): Camera =
    Camera(Vector(), Vector(x = fovX / 2), Vector(y = -fovY / 2), Vector(z = 1.0))

case class ScreenSize(width: Int, height: Int)
