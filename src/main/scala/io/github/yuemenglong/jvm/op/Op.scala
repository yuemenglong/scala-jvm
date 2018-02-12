package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{ClassFile, JvmItem, StreamReader}
import io.github.yuemenglong.jvm.struct.MethodInfo

import scala.collection.mutable.ArrayBuffer
import io.github.yuemenglong.jvm.common.Types._

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */

class RtCtx {
  var stack = new ArrayBuffer[Any]
}

trait Op extends JvmItem {
  val opCode: Int
  val method: MethodInfo

  val opName: String

  def proc(ctx: RtCtx)

  override def toString = opName
}

object Op {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, length: Int): Array[Op] = {
    val rest = reader.length - length
    val ret = new ArrayBuffer[Op]()
    while (reader.length > rest) {
      val code = (reader.readByte() + 256) % 256
      val op = code match {
        case c if 0x15 <= c && c <= 0x2D => OpLoad.load(reader, cf, method, code)
        case c if 0xAC <= c && c <= 0xB1 => OpReturn.load(reader, cf, method, code)
        case _ => new OpOther(reader, cf, method, code, reader.readBytes(reader.length.toInt - rest.toInt))
      }
      ret += op
    }
    ret.toArray
  }
}


class OpOther(reader: StreamReader,
              override val cf: ClassFile,
              override val method: MethodInfo,
              override val opCode: Int,
              val bytes: Array[Byte],
             ) extends Op {
  override val opName = (Array(opCode.toByte) ++ bytes).map(b => f"${b}%02X").mkString("-")

  override def proc(ctx: RtCtx): Unit = ???
}
