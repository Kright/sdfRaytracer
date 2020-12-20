package com.github.kright

import scala.annotation.tailrec
import scala.util.Random

object MyRandom:
  private val random = new Random()

  def nextDouble() = random.nextDouble()
  
  private def nextDoubleSymmetric(): Double = nextDouble() * 2.0 - 1.0

  @tailrec
  def nextPairInCircle(): (Double, Double) =
    val (dx, dy) = (nextDoubleSymmetric(), nextDoubleSymmetric())
    if (dx * dx + dy * dy <= 1.0)
      (dx, dy)
    else
      nextPairInCircle()

  @tailrec
  def nextRandomDirection(): NormalizedVector =
    def r() = random.nextDouble() * 2.0 - 1.0
    val v = Vector(nextDoubleSymmetric(), nextDoubleSymmetric(), nextDoubleSymmetric())
    if v.mag < 1.0
      v.normalized
    else
      nextRandomDirection()
      
  def nextRandomColor(): Color = Color(nextDouble(), nextDouble(), nextDouble())

  def selectRandom[T](probFirst: Double, first: T, second: T): T =
    if (nextDouble() < probFirst) first else second
      

  