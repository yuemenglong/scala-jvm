package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{ClassFile, JvmItem, StreamReader}
import io.github.yuemenglong.jvm.struct.MethodInfo

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
  require(n >= 0 && n <= 3)

  private def n = opCode - 0x2A

  private def idx = n + 1

  override val opName = s"aload_${n}"

  override def proc(ctx: RtCtx): Unit = {

  }
}
