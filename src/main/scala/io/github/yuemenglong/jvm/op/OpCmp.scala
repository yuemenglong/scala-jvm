package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.reflect.{ClassTag, classTag}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpCmp {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, code: Int): Op = {
    code match {
      case c if 0x99 <= c && c <= 0x9E => new OpCmp0(reader, cf, method, code)
      case c if 0x9F <= c && c <= 0xA4 => new OpCmpI(reader, cf, method, code)
      case c if 0xA5 <= c && c <= 0xA6 => new OpCmpA(reader, cf, method, code)
    }
  }
}

class OpCmp0(val reader: StreamReader,
             override val cf: ClassFile,
             override val method: MethodInfo,
             val opCode: Int,
            ) extends Op {
  val offset: Short = reader.readShort()

  val fn: Int => Boolean = opCode match {
    case 0x99 => _ == 0
    case 0x9A => _ != 0
    case 0x9B => _ < 0
    case 0x9C => _ >= 0
    case 0x9D => _ > 0
    case 0x9E => _ <= 0
  }
  val postfix = opCode match {
    case 0x99 => "eq"
    case 0x9A => "ne"
    case 0x9B => "lt"
    case 0x9C => "ge"
    case 0x9D => "gt"
    case 0x9E => "le"
  }
  override val opName = {
    s"if${postfix} ${offset}"
  }

  override def proc(ctx: RtCtx): Unit = ???
}

class OpCmpI(val reader: StreamReader,
             override val cf: ClassFile,
             override val method: MethodInfo,
             val opCode: Int,
            ) extends Op {
  val offset: Short = reader.readShort()
  val fn: (Int, Int) => Boolean = opCode match {
    case 0x9F => (a: Int, b: Int) => a == b
    case 0xA0 => (a: Int, b: Int) => a != b
    case 0xA1 => (a: Int, b: Int) => a < b
    case 0xA2 => (a: Int, b: Int) => a >= b
    case 0xA3 => (a: Int, b: Int) => a > b
    case 0xA4 => (a: Int, b: Int) => a <= b
  }
  val postfix = opCode match {
    case 0x9F => "eq"
    case 0xA0 => "ne"
    case 0xA1 => "lt"
    case 0xA2 => "ge"
    case 0xA3 => "gt"
    case 0xA4 => "le"
  }
  override val opName = {
    s"if_icmp${postfix} ${offset}"
  }

  override def proc(ctx: RtCtx): Unit = ???
}


class OpCmpA(val reader: StreamReader,
             override val cf: ClassFile,
             override val method: MethodInfo,
             val opCode: Int,
            ) extends Op {
  val offset: Short = reader.readShort()
  val fn: (AnyRef, AnyRef) => Boolean = opCode match {
    case 0xA5 => (a: AnyRef, b: AnyRef) => a == b
    case 0xA6 => (a: AnyRef, b: AnyRef) => a != b
  }
  val postfix = opCode match {
    case 0xA5 => "eq"
    case 0xA6 => "ne"
  }

  override val opName = {
    s"if_acmp${postfix} ${offset}"
  }

  override def proc(ctx: RtCtx): Unit = ???
}
