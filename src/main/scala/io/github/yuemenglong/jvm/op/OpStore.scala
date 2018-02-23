package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.StreamReader
import io.github.yuemenglong.jvm.nativ.{Arr, Num, Ref}
import io.github.yuemenglong.jvm.rt.ThreadCtx
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpStore {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case c if 0x36 <= c && c <= 0x4E => new OpStore(reader, cf, method, lineNo, code)
      case c if 0x3F <= c && c <= 0x56 => new OpAStore(reader, cf, method, lineNo, code)
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
    case c if 0x3B <= c && c <= 0x4E => (opCode - 0x3B) % 4
  }

  val prefix = opCode match {
    case c if 0x36 <= c && c <= 0x3A => "ilfda".charAt(opCode - 0x36)
    case c if 0x3B <= c && c <= 0x4E => "ilfda".charAt((opCode - 0x3B) / 4)
  }

  override val opName = {
    s"${prefix}store_${index}"
  }

  override def proc(ctx: ThreadCtx): Unit = {
    val value = ctx.pop()
    ctx.set(index, value)
  }
}

class OpAStore(reader: StreamReader,
               override val cf: ClassFile,
               override val method: MethodInfo,
               val lineNo: Int,
               val opCode: Int
              ) extends Op {
  val prefix = "ilfdabcs".charAt(opCode - 0x4F)
  override val opName = s"${prefix}astore"

  override def proc(ctx: ThreadCtx): Unit = {
    val value = ctx.pop()
    val index = ctx.pop().toString.toInt
    val arr = ctx.pop().asInstanceOf[Arr[_]]
    val v = value match {
      case r: Ref => r
      case _ => Num.convert(value, arr.clazz)
    }
    arr.store(index, v)
  }
}
