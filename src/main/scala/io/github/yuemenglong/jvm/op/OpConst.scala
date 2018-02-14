package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.jvm.rt.ThreadCtx
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.reflect.{ClassTag, classTag}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpConst {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case c if 0x00 <= c && c <= 0x00 => new OpNop(reader, cf, method, lineNo, code)
      case c if 0x01 <= c && c <= 0x0F => new OpConst(reader, cf, method, lineNo, code)
      case c if 0x10 <= c && c <= 0x11 => new OpPush(reader, cf, method, lineNo, code)
      case c if 0x12 <= c && c <= 0x14 => new OpLdc(reader, cf, method, lineNo, code)
    }
  }
}

class OpNop(val reader: StreamReader,
            override val cf: ClassFile,
            override val method: MethodInfo,
            val lineNo: Int,
            val opCode: Int,
           ) extends Op {
  override val opName = {
    "nop"
  }

  override def proc(ctx: ThreadCtx): Unit = {}
}

class OpConst(reader: StreamReader,
              override val cf: ClassFile,
              override val method: MethodInfo,
              val lineNo: Int,
              val opCode: Int,
             ) extends Op {
  def value = opCode match {
    case 0x01 => null
    case 0x02 => -1
    case 0x03 => 0
    case 0x04 => 1
    case 0x05 => 2
    case 0x06 => 3
    case 0x07 => 4
    case 0x08 => 5
    case 0x09 => 0.toLong
    case 0x0A => 1.toLong
    case 0x0B => 0.toFloat
    case 0x0C => 1.toFloat
    case 0x0D => 2.toFloat
    case 0x0E => 0.toDouble
    case 0x0F => 1.toDouble
  }

  override val opName = {
    opCode match {
      case 0x01 => "aconst_null"
      case 0x02 => "iconst_m1"
      case c if 0x03 <= c && c <= 0x08 => s"iconst_${c - 0x03}"
      case c if 0x09 <= c && c <= 0x0A => s"lconst_${c - 0x03}"
      case c if 0x0B <= c && c <= 0x0D => s"fconst_${c - 0x0B}"
      case c if 0x0E <= c && c <= 0x0F => s"dconst_${c - 0x0E}"
    }
  }

  override def proc(ctx: ThreadCtx): Unit = ctx.push(value)
}

class OpPush(reader: StreamReader,
             override val cf: ClassFile,
             override val method: MethodInfo,
             val lineNo: Int,
             val opCode: Int,
            ) extends Op {
  val value = opCode match {
    case 0x10 => reader.readByte()
    case 0x11 => reader.readShort()
  }
  override val opName = opCode match {
    case 0x10 => s"bipush ${value}"
    case 0x11 => s"sipush ${value}"
  }

  override def proc(ctx: ThreadCtx): Unit = ctx.push(value)
}


class OpLdc(reader: StreamReader,
            override val cf: ClassFile,
            override val method: MethodInfo,
            val lineNo: Int,
            val opCode: Int,
           ) extends Op {
  val index = opCode match {
    case 0x12 => reader.readByte()
    case 0x13 => reader.readShort()
    case 0x14 => reader.readShort()
  }
  override val opName = opCode match {
    case 0x12 => s"ldc ${cp(index)}"
    case 0x13 => s"ldc_w ${cp(index)}"
    case 0x14 => s"ldc2_w ${cp(index)}"
  }

  override def proc(ctx: ThreadCtx): Unit = ctx.push(cpv(index).value)
}
