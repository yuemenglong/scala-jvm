package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.reflect.{ClassTag, classTag}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpDup {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, code: Int): Op = {
    code match {
      case c if 0x59 <= c && c <= 0x5B => new OpDup(reader, cf, method, code)
      case c if 0x5C <= c && c <= 0x5E => new OpDup2(reader, cf, method, code)
      case 0x5F => new OpSwap(reader, cf, method, code)
    }
  }
}

class OpDup(val reader: StreamReader,
            override val cf: ClassFile,
            override val method: MethodInfo,
            val opCode: Int,
           ) extends Op {
  val offset: Int = opCode - 0x59

  override val opName = {
    opCode match {
      case 0x59 => "dup"
      case 0x5A => "dup_x1"
      case 0x5B => "dup_x2"
    }
  }

  override def proc(ctx: RtCtx): Unit = ???
}

class OpDup2(val reader: StreamReader,
             override val cf: ClassFile,
             override val method: MethodInfo,
             val opCode: Int,
            ) extends Op {
  val offset: Int = opCode - 0x5C

  override val opName = {
    opCode match {
      case 0x5C => "dup2"
      case 0x5D => "dup2_x1"
      case 0x5E => "dup2_x2"
    }
  }

  override def proc(ctx: RtCtx): Unit = ???
}


class OpSwap(val reader: StreamReader,
             override val cf: ClassFile,
             override val method: MethodInfo,
             val opCode: Int,
            ) extends Op {
  val offset: Int = opCode - 0x5C

  override val opName = {
    "swap"
  }

  override def proc(ctx: RtCtx): Unit = ???
}