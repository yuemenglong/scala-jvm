package io.github.yuemenglong.jvm.nativ

import scala.reflect.ClassTag

/**
  * Created by <yuemenglong@126.com> on 2018/2/22.
  */

object Num {
  def add(a: Any, b: Any): Any = {
    require(a.getClass == b.getClass)
    (a, b) match {
      case (Float.NaN, _) => Float.NaN
      case (Double.NaN, _) => Double.NaN
      case (_, Float.NaN) => Float.NaN
      case (_, Double.NaN) => Double.NaN
      case (a: Byte, b: Byte) => a + b
      case (a: Short, b: Short) => a + b
      case (a: Int, b: Int) => a + b
      case (a: Long, b: Long) => a + b
      case (a: Float, b: Float) => a + b
      case (a: Double, b: Double) => a + b
      case _ => throw new RuntimeException("Unreachable")
    }
  }

  def sub(a: Any, b: Any): Any = {
    require(a.getClass == b.getClass)
    (a, b) match {
      case (Float.NaN, _) => Float.NaN
      case (Double.NaN, _) => Double.NaN
      case (_, Float.NaN) => Float.NaN
      case (_, Double.NaN) => Double.NaN
      case (a: Byte, b: Byte) => a - b
      case (a: Short, b: Short) => a - b
      case (a: Int, b: Int) => a - b
      case (a: Long, b: Long) => a - b
      case (a: Float, b: Float) => a - b
      case (a: Double, b: Double) => a - b
      case _ => throw new RuntimeException("Unreachable")
    }
  }

  def mul(a: Any, b: Any): Any = {
    require(a.getClass == b.getClass)
    (a, b) match {
      case (Float.NaN, _) => Float.NaN
      case (Double.NaN, _) => Double.NaN
      case (_, Float.NaN) => Float.NaN
      case (_, Double.NaN) => Double.NaN
      case (a: Byte, b: Byte) => a * b
      case (a: Short, b: Short) => a * b
      case (a: Int, b: Int) => a * b
      case (a: Long, b: Long) => a * b
      case (a: Float, b: Float) => a * b
      case (a: Double, b: Double) => a * b
      case _ => throw new RuntimeException("Unreachable")
    }
  }

  def div(a: Any, b: Any): Any = {
    require(a.getClass == b.getClass)
    (a, b) match {
      case (Float.NaN, _) => Float.NaN
      case (Double.NaN, _) => Double.NaN
      case (_, Float.NaN) => Float.NaN
      case (_, Double.NaN) => Double.NaN
      case (a: Byte, b: Byte) => a / b
      case (a: Short, b: Short) => a / b
      case (a: Int, b: Int) => a / b
      case (a: Long, b: Long) => a / b
      case (a: Float, b: Float) => a / b
      case (a: Double, b: Double) => a / b
      case _ => throw new RuntimeException("Unreachable")
    }
  }

  def cmp(a: Any, b: Any, nan: Int = -1): Int = {
    require(a.getClass == b.getClass)
    (a, b) match {
      case (Float.NaN, _) => nan
      case (Double.NaN, _) => nan
      case (_, Float.NaN) => nan
      case (_, Double.NaN) => nan
      case (a: Byte, b: Byte) => if (a < b) -1 else if (a > b) 1 else 0
      case (a: Short, b: Short) => if (a < b) -1 else if (a > b) 1 else 0
      case (a: Int, b: Int) => if (a < b) -1 else if (a > b) 1 else 0
      case (a: Long, b: Long) => if (a < b) -1 else if (a > b) 1 else 0
      case (a: Float, b: Float) => if (a < b) -1 else if (a > b) 1 else 0
      case (a: Double, b: Double) => if (a < b) -1 else if (a > b) 1 else 0
      case _ => throw new RuntimeException("Unreachable")
    }
  }

  def convert(v: Any, clazz: Class[_]): Any = {
    if (clazz == classOf[Byte]) {
      v.toString.toByte
    } else if (clazz == classOf[Char]) {
      v.toString.toInt.toChar
    } else if (clazz == classOf[Short]) {
      v.toString.toShort
    } else if (clazz == classOf[Int]) {
      v.toString.toInt
    } else if (clazz == classOf[Long]) {
      v.toString.toLong
    } else if (clazz == classOf[Float]) {
      v.toString.toFloat
    } else if (clazz == classOf[Double]) {
      v.toString.toDouble
    } else {
      ???
    }
  }
}
