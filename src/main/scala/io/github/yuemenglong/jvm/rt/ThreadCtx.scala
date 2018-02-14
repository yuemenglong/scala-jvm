package io.github.yuemenglong.jvm.rt

import io.github.yuemenglong.jvm.struct.MethodInfo

import scala.collection.mutable.ArrayBuffer

/**
  * Created by <yuemenglong@126.com> on 2018/2/14.
  */
class ThreadCtx(val method: MethodInfo, val rt: RuntimeCtx) {

  var frames: ArrayBuffer[Frame] = new ArrayBuffer[Frame]()
  var stack: ArrayBuffer[Any] = new ArrayBuffer[Any]()
  frames += new Frame(method)

  def pc = frame.pc

  def frame = frames.last

  def push(value: Any): Unit = stack += value

  def pop(): Any = {
    val ret = stack.last
    stack -= ret
    ret
  }

  def get(idx: Int): Any = frame.get(idx)

  def set(idx: Int, value: Any): Unit = frame.set(idx, value)

  def call(method: MethodInfo, params: Map[Int, Any]): Unit = {
    val frame = new Frame(method, params)
    frames += frame
  }

  def ret() = {
    frames -= frame
  }

  def inc(): Unit = {
    frame.pc += 1
  }

  override def toString = {
    val l = frame.localVariable.toArray.sortBy(_._1).map { case (idx, value) =>
      s"\t[Local] [${idx}] ${value}"
    }.mkString("\n")
    val s = stack.map(v => s"\t[Stack] ${v}").mkString("\n")
    s"${l}\n${s}"
  }
}
