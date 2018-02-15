package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.jvm.rt.ThreadCtx
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpConvert {
  def load(reader: StreamReader, cf: ClassFile,
           method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case c if 0x85 <= c && c <= 0x93 => new OpConvert(reader, cf, method, lineNo, code)
    }
  }
}

class OpConvert(val reader: StreamReader,
                override val cf: ClassFile,
                override val method: MethodInfo,
                val lineNo: Int,
                val opCode: Int,
               ) extends Op {
  val prefix: Char = "ilfdi".charAt((opCode - 0x85) / 3)
  val postfix: Char = "lfdifdildilfbcs".charAt(opCode - 0x85)
  override val opName = s"${prefix}2${postfix}"

  override def proc(ctx: ThreadCtx): Unit = {
    val value = prefix match {
      case 'i' => ctx.pop().asInstanceOf[Int]
      case 'l' => ctx.pop().asInstanceOf[Long]
      case 'f' => ctx.pop().asInstanceOf[Float]
      case 'd' => ctx.pop().asInstanceOf[Double]
    }
    val res = postfix match {
      case 'i' => value.toInt
      case 'l' => value.toLong
      case 'f' => value.toFloat
      case 'd' => value.toDouble
      case 'b' => value.toByte
      case 'c' => value.toChar
      case 's' => value.toShort
    }
    ctx.push(res)
  }
}
