package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.reflect.{ClassTag, classTag}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpCtrl {
  def load(reader: StreamReader, cf: ClassFile,
           method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case 0xA7 => new OpGoto(reader, cf, method, lineNo, code)
    }
  }
}

class OpGoto(val reader: StreamReader,
             override val cf: ClassFile,
             override val method: MethodInfo,
             val lineNo: Int,
             val opCode: Int,
            ) extends Op {
  val offset = reader.readShort()
  override val opName = {
    s"goto ${offset}"
  }

  override def proc(ctx: RtCtx): Unit = ???
}
