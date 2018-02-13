package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.jvm.rt.ThreadCtx
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.reflect.{ClassTag, classTag}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpReturn {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case c if 0xAC <= c && c <= 0xB1 => new OpReturn(reader, cf, method, lineNo, code)
    }
  }
}

class OpReturn(val reader: StreamReader,
               override val cf: ClassFile,
               override val method: MethodInfo,
               val lineNo: Int,
               val opCode: Int,
              ) extends Op {
  val prefix = opCode match {
    case 0xAC => "i"
    case 0xAD => "l"
    case 0xAE => "f"
    case 0xAF => "d"
    case 0xB0 => "a"
    case 0xB1 => ""
  }
  override val opName = {
    s"${prefix}return"
  }

  override def proc(ctx: ThreadCtx): Unit = {
    ctx.ret()
  }
}
