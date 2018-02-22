package io.github.yuemenglong.jvm.nativ

/**
  * Created by <yuemenglong@126.com> on 2018/2/22.
  */

object Num {
  def add(a: Num, b: Num): Num = {
    require(a.ty == b.ty)
    val value = (a.value, b.value) match {
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
    }
    new Num(value)
  }

  def sub(a: Num, b: Num): Num = {
    require(a.ty == b.ty)
    val value = (a.value, b.value) match {
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
    }
    new Num(value)
  }

  def mul(a: Num, b: Num): Num = {
    require(a.ty == b.ty)
    val value = (a.value, b.value) match {
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
    }
    new Num(value)
  }

  def div(a: Num, b: Num): Num = {
    require(a.ty == b.ty)
    val value = (a.value, b.value) match {
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
    }
    new Num(value)
  }

  def cmp(a: Num, b: Num): Int = {
    require(a.ty == b.ty && !a.isNaN && !b.isNaN)
    (a.value, b.value) match {
      case (a: Byte, b: Byte) => if (a < b) -1 else if (a > b) 1 else 0
      case (a: Short, b: Short) => if (a < b) -1 else if (a > b) 1 else 0
      case (a: Int, b: Int) => if (a < b) -1 else if (a > b) 1 else 0
      case (a: Long, b: Long) => if (a < b) -1 else if (a > b) 1 else 0
      case (a: Float, b: Float) => if (a < b) -1 else if (a > b) 1 else 0
      case (a: Double, b: Double) => if (a < b) -1 else if (a > b) 1 else 0
    }
  }
}

class Num(val value: Any) {
  val ty: String = value match {
    case _: Byte => "Byte"
    case _: Short => "Short"
    case _: Int => "Int"
    case _: Long => "Long"
    case _: Float => "Float"
    case _: Double => "Double"
  }

  val isNaN = value match {
    case Float.NaN | Double.NaN => true
    case _ => false
  }

  def add(n: Num): Num = Num.add(this, n)

  def sub(n: Num): Num = Num.sub(this, n)

  def mul(n: Num): Num = Num.mul(this, n)

  def div(n: Num): Num = Num.div(this, n)

  def cmp(n: Num): Int = Num.cmp(this, n)
}
