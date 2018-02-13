package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{JvmItem, StreamReader}
import io.github.yuemenglong.jvm.rt.ThreadCtx
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.collection.mutable.ArrayBuffer



trait Op extends JvmItem {
  val method: MethodInfo
  val opCode: Int
  val lineNo: Int

  val opName: String

  def proc(ctx: ThreadCtx)

  override def toString = opName
}

object Op {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, length: Int): Array[Op] = {
    val rest = reader.length - length
    val ret = new ArrayBuffer[Op]()
    while (reader.length > rest) {
      val lineNo = ret.length
      val code = (reader.readByte() + 256) % 256
      val op = code match {
        case c if 0x01 <= c && c <= 0x14 => OpPush.load(reader, cf, method, lineNo, code)
        case c if 0x15 <= c && c <= 0x2D => OpLoad.load(reader, cf, method, lineNo, code)
        case c if 0x36 <= c && c <= 0x4E => OpStore.load(reader, cf, method, lineNo, code)
        case c if 0x59 <= c && c <= 0x5F => OpDup.load(reader, cf, method, lineNo, code)
        case c if 0x60 <= c && c <= 0x84 => OpMath.load(reader, cf, method, lineNo, code)
        case c if 0x94 <= c && c <= 0xA6 => OpCmp.load(reader, cf, method, lineNo, code)
        case c if 0xA7 <= c && c <= 0xA7 => OpCtrl.load(reader, cf, method, lineNo, code)
        case c if 0xAC <= c && c <= 0xB1 => OpReturn.load(reader, cf, method, lineNo, code)
        case c if 0xB6 <= c && c <= 0xBA => OpInvoke.load(reader, cf, method, lineNo, code)
        case c if 0xBB <= c && c <= 0xBD => OpNew.load(reader, cf, method, lineNo, code)
        case c if 0xBF <= c && c <= 0xBF => OpUtil.load(reader, cf, method, lineNo, code)
        case _ => new OpOther(reader, cf, method, lineNo, code, reader.readBytes(reader.length.toInt - rest.toInt))
      }
      ret += op
    }
    ret.toArray
  }
}


class OpOther(reader: StreamReader,
              override val cf: ClassFile,
              override val method: MethodInfo,
              override val lineNo: Int,
              val opCode: Int,
              val bytes: Array[Byte],
             ) extends Op {
  override val opName = (Array(opCode.toByte) ++ bytes).map(b => f"${b}%02X").mkString("-")

  override def proc(ctx: ThreadCtx): Unit = ???
}
