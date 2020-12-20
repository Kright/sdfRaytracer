package com.github.kright

trait Material:
  def getColor(pos: Vector, incomingDir: NormalizedVector, normal: NormalizedVector, rayTracer: RayTracer, bouncesCount: Int): Color

class EmissiveColor(color: Color) extends Material:
  override def getColor(pos: Vector, incomingDir: NormalizedVector, normal: NormalizedVector, rayTracer: RayTracer, bouncesCount: Int): Color = color

class Mirror(albedo: Color) extends Glossy(albedo, roughness = 0.0)

class Diffuse(albedo: Color) extends Glossy(albedo, roughness = 1.0)

class Glossy(albedo: Color, roughness: Double) extends Material:
  require(roughness >= 0.0 && roughness <= 1.0)
  override def getColor(pos: Vector, incomingDir: NormalizedVector, normal: NormalizedVector, rayTracer: RayTracer, bouncesCount: Int): Color =
    val randomDiffuse = MyRandom.nextRandomDirection() + normal
    val reflected = incomingDir.reflected(normal)
    val outcomingDir = randomDiffuse * roughness + reflected * (1.0 - roughness)
    val incomingLight = rayTracer.render(Ray(pos + normal * rayTracer.eps * 10.0, outcomingDir), bouncesCount).getOrElse(Color.black)
    albedo * incomingLight

class Checkers(step: Double, white: Material, black: Material) extends Material:
  override def getColor(pos: Vector, incomingDir: NormalizedVector, normal: NormalizedVector, rayTracer: RayTracer, bouncesCount: Int): Color =
    val p = pos / step
    val current = if ((math.floor(p.x) + math.floor(p.y) + math.floor(p.z)).toInt % 2 == 0) white else black
    current.getColor(pos, incomingDir, normal, rayTracer, bouncesCount)

// https://learnopengl.com/PBR/Theory
class FresnelShlick(f0: Double = 0.04, reflective: Material, original: Material) extends Material:
  override def getColor(pos: Vector, incomingDir: NormalizedVector, normal: NormalizedVector, rayTracer: RayTracer, bouncesCount: Int): Color =
    val cosTheta = math.abs(incomingDir.normalized dot normal.normalized)
    val t = f0 + (1.0 - f0) * math.pow(1.0 - cosTheta, 5.0)
    MyRandom.selectRandom(t, reflective, original).getColor(pos, incomingDir, normal, rayTracer, bouncesCount)

// sin(a) / sin(b) = n
class Refraction(ior: Double) extends Material:
  var innerRaytracer: RayTracer = null
  override def getColor(pos: Vector, incomingDir: NormalizedVector, normal: NormalizedVector, rayTracer: RayTracer, bouncesCount: Int): Color = 
    require(innerRaytracer != null)
    val inNormalProjection = (incomingDir dot normal) * normal
    val sinIn: Vector = incomingDir - inNormalProjection
    val sinOut: Vector = sinIn / ior
    if (sinOut.magSqr >= 1.0) 
      // full inner reflection
      val reflected = incomingDir.reflected(normal)
      rayTracer.render(Ray(pos + normal * rayTracer.eps * 10, reflected), bouncesCount).getOrElse(Color.black)
    else 
      val cosOut: Double = math.sqrt(1.0 - sinOut.magSqr)
      val outNormalProjection: Vector = inNormalProjection * (cosOut / inNormalProjection.mag)
      val outDir: NormalizedVector = NormalizedVector(outNormalProjection + sinOut)
      innerRaytracer.render(Ray(pos - normal * innerRaytracer.eps * 10, outDir), bouncesCount).getOrElse(Color.black)

  def inverted = Refraction(1.0 / ior)
      
  
