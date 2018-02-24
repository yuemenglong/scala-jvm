package io.github.yuemenglong.jvm.nativ

import io.github.yuemenglong.jvm.rt.Vm

import scala.reflect.ClassTag

/**
  * Created by <yuemenglong@126.com> on 2018/2/23.
  */
class Arr[T: ClassTag](size: Int, init: Array[T] = null) extends Ref {
  def this(init: Array[T]) = this(init.length, init)

  val array = init match {
    case null => new Array[T](size)
    case _ => init
  }
  val clazz = implicitly[ClassTag[T]].runtimeClass

  def store(idx: Int, value: Any): Unit = {
    array(idx) = value.asInstanceOf[T]
  }

  def load(idx: Int): Any = array(idx)

  override def toString = {
    s"Arr[${clazz}]@[${id}]"
  }
}
