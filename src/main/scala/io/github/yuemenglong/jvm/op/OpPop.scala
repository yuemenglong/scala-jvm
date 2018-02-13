package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.jvm.rt.ThreadCtx
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.reflect.{ClassTag, classTag}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpPop {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case c if 0x57 <= c && c <= 0x58 => new OpPop(reader, cf, method, lineNo, code)
    }
  }
}

class OpPop(reader: StreamReader,
            override val cf: ClassFile,
            override val method: MethodInfo,
            val lineNo: Int,
            val opCode: Int,
           ) extends Op {
  override val opName = {
    opCode match {
      case 0x57 => "pop"
      case 0x58 => "pop2"
    }
  }

  override def proc(ctx: ThreadCtx): Unit = ctx.pop()
}
