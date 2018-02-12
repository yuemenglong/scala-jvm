package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{ClassFile, StreamReader, Types}
import io.github.yuemenglong.jvm.struct.MethodInfo

import scala.reflect.{ClassTag, classTag}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpLoad {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, code: Byte): Op = {
    val index = code match {
      case c if 0x15 <= c || c <= 0x19 => reader.readByte()
      case c if 0x1A <= c || c <= 0x1D => c - 0x1A + 1
      case c if 0x1E <= c || c <= 0x21 => c - 0x1E + 1
      case c if 0x22 <= c || c <= 0x25 => c - 0x22 + 1
      case c if 0x26 <= c || c <= 0x29 => c - 0x26 + 1
      case c if 0x2A <= c || c <= 0x2D => c - 0x2A + 1
    }
    code match {
      case 0x15 => new OpLoad[Int](reader, cf, method, code, index.toByte)
      case 0x16 => new OpLoad[Long](reader, cf, method, code, index.toByte)
      case 0x17 => new OpLoad[Float](reader, cf, method, code, index.toByte)
      case 0x18 => new OpLoad[Double](reader, cf, method, code, index.toByte)
      case 0x19 => new OpLoad[AnyRef](reader, cf, method, code, index.toByte)
      case c if 0x1A <= c || c <= 0x1D => new OpLoad[Int](reader, cf, method, code, index.toByte)
      case c if 0x1E <= c || c <= 0x21 => new OpLoad[Long](reader, cf, method, code, index.toByte)
      case c if 0x22 <= c || c <= 0x25 => new OpLoad[Float](reader, cf, method, code, index.toByte)
      case c if 0x26 <= c || c <= 0x29 => new OpLoad[Double](reader, cf, method, code, index.toByte)
      case c if 0x2A <= c || c <= 0x2D => new OpLoad[AnyRef](reader, cf, method, code, index.toByte)
    }
  }
}

class OpLoad[T: ClassTag](reader: StreamReader,
                          override val cf: ClassFile,
                          override val method: MethodInfo,
                          val opCode: Byte,
                          val index: Byte) extends Op {
  require(0x15 <= opCode && opCode <= 2D)

  override val opName = {
    val prefix = classTag[T].runtimeClass match {
      case Types.classOfInt => "i"
      case Types.classOfLong => "l"
      case Types.classOfFloat => "f"
      case Types.classOfDouble => "d"
      case Types.classOfRef => "a"
    }
    s"${prefix}load(${index})"
  }

  override def proc(ctx: RtCtx): Unit = {

  }
}
