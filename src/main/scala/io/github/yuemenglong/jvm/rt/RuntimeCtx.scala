package io.github.yuemenglong.jvm.rt

import io.github.yuemenglong.jvm.op.Op
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */

class RuntimeCtx {
  var heap: Any = _
  var clazzMap: Map[String, ClassFile] = Map()
  var threads: ArrayBuffer[ThreadCtx] = new ArrayBuffer[ThreadCtx]()

  def newThread(method: MethodInfo): ThreadCtx = {
    threads += new ThreadCtx(method, this)
    threads.last
  }
}

class ThreadCtx(val method: MethodInfo, val rt: RuntimeCtx) {
  var frames: ArrayBuffer[Frame] = new ArrayBuffer[Frame]()
  frames += new Frame(method)

  def pc = frame.pc

  def frame = frames.last

  def push(value: Any): Unit = frame.push(value)

  def pop(): Any = frame.pop()

  def get(idx: Int): Any = frame.get(idx)

  def set(idx: Int, value: Any): Unit = frame.set(idx, value)

  def call(method: MethodInfo): Unit = {
    val frame = new Frame(method)
    frames += frame
  }

  def inc(): Unit = {
    frame.pc += 1
  }
}

class Frame(val method: MethodInfo) {
  var pc: Int = 0
  var localVariable: Map[Int, Any] = Map()
  var stack: ArrayBuffer[Any] = new ArrayBuffer[Any]()

  def push(value: Any): Unit = {
    stack += value
  }

  def pop(): Any = {
    val ret = stack.last
    stack -= ret
    ret
  }

  def get(idx: Int): Any = localVariable(idx)

  def set(idx: Int, value: Any): Unit = localVariable += (idx -> value)

  def code(pc: Int) = method.code.code(pc)

  override def toString = {
    val l = localVariable.toArray.sortBy(_._1).map { case (idx, value) =>
      s"[Local] [${idx}] ${value}"
    }.mkString("\n")
    val s = stack.map(v => s"[Stack] ${v}").mkString("\n")
    s"${l}\n${s}"
  }
}


