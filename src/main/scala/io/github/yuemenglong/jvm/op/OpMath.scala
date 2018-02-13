package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.reflect.{ClassTag, classTag}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpMath {
  def load(reader: StreamReader, cf: ClassFile,
           method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case c if 0x60 <= c && c <= 0x77 => new OpMath(reader, cf, method, lineNo, code)
      case c if 0x78 <= c && c <= 0x84 => new OpMath2(reader, cf, method, lineNo, code)
    }
  }
}

class OpMath(val reader: StreamReader,
             override val cf: ClassFile,
             override val method: MethodInfo,
             val lineNo: Int,
             val opCode: Int,
            ) extends Op {
  override val opName = {
    val fn = opCode match {
      case c if 0x60 <= c && c <= 0x63 => "add"
      case c if 0x64 <= c && c <= 0x67 => "sub"
      case c if 0x68 <= c && c <= 0x6B => "mul"
      case c if 0x6C <= c && c <= 0x6F => "div"
      case c if 0x70 <= c && c <= 0x73 => "rem"
      case c if 0x74 <= c && c <= 0x77 => "neg"
    }
    val prefix = "ilfd".charAt(opCode % 4)
    s"${prefix}${fn}"
  }

  override def proc(ctx: RtCtx): Unit = ???
}

class OpMath2(val reader: StreamReader,
              override val cf: ClassFile,
              override val method: MethodInfo,
              val lineNo: Int,
              val opCode: Int,
             ) extends Op {
  override val opName = {
    val fn = opCode match {
      case c if 0x78 <= c && c <= 0x79 => "shl"
      case c if 0x7A <= c && c <= 0x7B => "shr"
      case c if 0x7C <= c && c <= 0x7D => "ushr"
      case c if 0x7E <= c && c <= 0x7F => "and"
      case c if 0x80 <= c && c <= 0x81 => "or"
      case c if 0x82 <= c && c <= 0x83 => "xor"
      case c if 0x84 <= c && c <= 0x84 => "inc"
    }
    val prefix = "il".charAt(opCode % 2)
    s"${prefix}${fn}"
  }

  override def proc(ctx: RtCtx): Unit = ???
}