package com.enricode.util

trait LeetcodeApp extends App {
  def run(): Unit
  val start = System.currentTimeMillis()
  run()
  val runtime = System.currentTimeMillis() - start
  println(s"runtime: ${runtime}ms")
}
