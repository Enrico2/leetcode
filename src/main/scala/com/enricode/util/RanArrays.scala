package com.enricode.util

object RanArrays {
  def inBounds(i: Int, max: Int, min: Int = 0): Boolean = i >= min && i < max
}
