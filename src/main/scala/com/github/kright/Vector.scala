package com.github.kright

import java.security.{CryptoPrimitive, SecureRandom}

import scala.annotation.tailrec
import scala.util.Random

open case class Vector(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0):
  def +(v:Vector) = Vector(x + v.x, y + v.y, z + v.z)
  def -(v:Vector) = Vector(x - v.x, y - v.y, z - v.z)
  def *(d:Double) = Vector(x * d, y * d, z * d)
  def /(d:Double) = this * (1.0 / d)
  def unary_- = Vector(-x, -y, -z)

  def dot(v:Vector) = x * v.x + y * v.y + z * v.z
  def magSqr: Double = this dot this
  def mag: Double = math.sqrt(magSqr)
  def normalized: NormalizedVector = NormalizedVector(this) 

  def reflected(normal: Vector) = this + normal * ((this dot normal) * -2)

def (m: Double) * (v: Vector) = v * m


class NormalizedVector private(v: Vector) extends Vector(v.x, v.y, v.z)

object NormalizedVector:
  def apply(v: Vector): NormalizedVector = new NormalizedVector(v / v.mag)

