package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.reflect.{ClassTag, classTag}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpStore {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case c if 0x36 <= c && c <= 0x4E => new OpStore(reader, cf, method, lineNo, code)
    }
  }
}

class OpStore(reader: StreamReader,
              override val cf: ClassFile,
              override val method: MethodInfo,
              val lineNo: Int,
              val opCode: Int
             ) extends Op {
  val index = opCode match {
    case c if 0x36 <= c && c <= 0x3A => reader.readByte().toInt
    case c if 0x3B <= c && c <= 0x4E => (opCode - 0x3B) % 4 + 1
  }

  override val opName = {
    val prefix = opCode match {
      case c if 0x36 <= c && c <= 0x3A => "ilfda".charAt(opCode - 0x36)
      case c if 0x3B <= c && c <= 0x4E => "ilfda".charAt((opCode - 0x3B) / 4)
    }
    s"${prefix}store ${index}"
  }

  override def proc(ctx: RtCtx): Unit = ???
}
