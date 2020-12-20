package com.github.kright

import scala.util.Random

trait RaysGenerator:
  def createRays(x: Int, y: Int)(using ScreenSize): Seq[Ray]

/**
 * @param camera
 * @param subpixelShifts is x,y pairs with values between [0.0, 1.0]
 */
class MSAAGenerator(camera: Camera, subpixelShifts: Seq[(Double, Double)]) extends RaysGenerator:
  override def createRays(x: Int, y: Int)(using screenSize: ScreenSize): Seq[Ray] =
    subpixelShifts.map { (sx, sy) =>
      val dx = 2.0 * (x + sx) / screenSize.width - 1.0
      val dy = 2.0 * (y + sy) / screenSize.height - 1.0
      val direction = camera.forward + camera.right * dx + camera.up * dy
      Ray(camera.pos, direction)
    }
  
    
object MSAAGenerator:
  val oneSample = Seq((0.5, 0.5))
  val msaa2x = Seq((0.25, 0.25), (0.75, 0.75))
  val msaa4x = Seq((0.625, 0.125), (0.125, 0.375), (0.875, 0.625), (0.375, 0.875))
  
  def random(count: Int): Seq[(Double, Double)] =
    (0 until count).map(_ => (MyRandom.nextDouble(), MyRandom.nextDouble())).toSeq

case class DOFGenerator(camera: Camera, 
                   focalLength: Double, 
                   lensSize: Double, 
                   lensRayShifts: () => (Double, Double),
                   generator: RaysGenerator) extends RaysGenerator:
  
  private val normalizedFwd = camera.forward.normalized
  
  override def createRays(x: Int, y: Int)(using screenSize: ScreenSize): Seq[Ray] = 
    generator.createRays(x, y).map { (ray: Ray) => 
      val cos: Double = ray.direction.normalized dot normalizedFwd
      val focusedPoint = ray.start + ray.direction.normalized * focalLength / cos
      val (sx, sy) = lensRayShifts()
      val newStart = ray.start + sx * lensSize * camera.right + sy * lensSize * camera.up
      Ray.fromTo(newStart, focusedPoint)
    }

