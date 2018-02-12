package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{ClassFile, JvmItem, StreamReader}
import io.github.yuemenglong.jvm.item.MethodInfo

import scala.collection.mutable.ArrayBuffer

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */

class RtCtx {
  var stack = new ArrayBuffer[Any]
}

trait Op extends JvmItem {
  val opCode: Byte
  val opName: String
  val method: MethodInfo

  def proc(ctx: RtCtx)
}

class OpAload(val reader: StreamReader,
              override val cf: ClassFile,
              override val method: MethodInfo,
              val opCode: Byte) extends Op {
  require(idx >= 0 && idx <= 3)

  private def idx = opCode - 0x2A

  override val opName = s"aload_${idx}"

  override def proc(ctx: RtCtx): Unit = {

  }
}
