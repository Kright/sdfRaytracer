package com.github.kright

open case class Color(r: Double, g: Double, b: Double):
  def *(m: Double) = Color(r * m, g * m, b * m)
  def /(d: Double) = this * (1.0 / d)
  def *(c: Color) = Color(r * c.r, g * c.g, b * c.b)
  def +(c: Color) = Color(r + c.r, g + c.g, b + c.b)
  def /(c: Color) = Color(r / c.r, g / c.g, b / c.b)
  def ^(gamma: Double) = Color(math.pow(r, gamma), math.pow(g, gamma), math.pow(b, gamma))
  
  def toRGBInt: Int = (toUByte(r) << 16) | (toUByte(g) << 8) | toUByte(b)
  private def toUByte(v: Double): Int = math.max(0, math.min(v.toInt, 255)) & 0xFF

object Color:
  val black = Color(0, 0, 0)