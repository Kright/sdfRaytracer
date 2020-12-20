package com.github.kright

import com.github.kright.Color

trait SDF:
  def apply(pos: Vector): Double

  def getNormal(pos: Vector): NormalizedVector =
    val eps = 0.001
    val f0 = apply(pos)
    Vector(
      apply(pos + Vector(x = eps)) - f0,
      apply(pos + Vector(y = eps)) - f0,
      apply(pos + Vector(z = eps)) - f0,
    ).normalized


class InvertedSDF(sdf: SDF) extends SDF:
  override def apply(pos: Vector): Double = -sdf(pos)

class Sphere(center: Vector, radius: Double) extends SDF: 
  override def apply(pos: Vector): Double = (pos - center).mag - radius
  override def getNormal(pos: Vector): NormalizedVector = (pos - center).normalized

class Ellipsis(val f1: Vector, val f2: Vector, val radius: Double) extends SDF:
  assert((f1 - f2).mag < radius)
  override def apply(pos: Vector): Double = 0.5 * ((f1 - pos).mag + (f2 - pos).mag - radius)

class Plane(start: Vector, normal: Vector) extends SDF:
  private val normalizedNorm = normal.normalized
  override def apply(pos: Vector): Double = (pos - start) dot normalizedNorm
  override def getNormal(pos: Vector): NormalizedVector = normalizedNorm

class Cube(center: Vector, r: Double) extends SDF:
  override def apply(pos: Vector): Double = 
    val v = pos - center
    math.max(math.abs(v.x), math.max(math.abs(v.y), math.abs(v.z))) - r 

class Intersection(models: Seq[SDF]) extends SDF:
  override def apply(pos: Vector): Double = 
    models.view.map(_(pos)).max
    
case class Combination(models: Seq[(SDF, Double)]) extends SDF:
  val totalW = models.map(_._2).map(math.abs).sum
  
  override def apply(pos: Vector): Double = 
    var result = 0.0
    for ((sdf, w) <- models) {
      result += sdf(pos) * w
    }
    result / totalW
 
object Lens:
  def apply(center: Vector, dc: Vector, r: Double) = Intersection(Seq[SDF](Sphere(center + dc, r), Sphere(center - dc, r)))

class Sin(val center: Vector, val w2pi: Vector, val amplitude: Double) extends SDF:
  private val maxGrad = math.max(1.0, amplitude * 6.28 / w2pi.mag) 
  override def apply(pos: Vector): Double = 
    val local = pos - center
    val phase = local dot w2pi
    val h = center.y + amplitude * math.sin(phase)
    (pos.y - h) * maxGrad  