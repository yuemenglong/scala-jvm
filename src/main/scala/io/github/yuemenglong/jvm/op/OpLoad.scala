package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.reflect.{ClassTag, classTag}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpLoad {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case c if 0x15 <= c && c <= 0x2D => new OpLoad(reader, cf, method, lineNo, code)
    }
  }
}

class OpLoad(reader: StreamReader,
             override val cf: ClassFile,
             override val method: MethodInfo,
             val lineNo: Int,
             val opCode: Int
            ) extends Op {
  val index = opCode match {
    case c if 0x15 <= c && c <= 0x19 => reader.readByte().toInt
    case c if 0x1A <= c && c <= 0x2D => (opCode - 0x1A) % 4 + 1
  }

  override val opName = {
    val prefix = opCode match {
      case c if 0x15 <= c && c <= 0x19 => "ilfda".charAt(opCode - 0x15)
      case c if 0x1A <= c && c <= 0x2D => "ilfda".charAt((opCode - 0x1A) / 4)
    }
    s"${prefix}load ${index}"
  }

  override def proc(ctx: RtCtx): Unit = ???
}
