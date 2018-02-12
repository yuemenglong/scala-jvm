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
  val opCode: Byte
  val opName: String
  val method: MethodInfo

  def proc(ctx: RtCtx)
}

object Op {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, length: Int): Array[Op] = {
    val rest = reader.length - length
    val ret = new ArrayBuffer[Op]()
    while (reader.length > rest) {
      val code = reader.readByte()
      val op = code match {
        case c if 0xAC <= c && c <= 0xB1 => OpReturn.load(reader, cf, method, code)
      }
      ret += op
    }
    ret.toArray
  }
}


