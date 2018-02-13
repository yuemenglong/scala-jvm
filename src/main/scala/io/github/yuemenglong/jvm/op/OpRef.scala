package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.reflect.{ClassTag, classTag}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpUtil {
  def load(reader: StreamReader, cf: ClassFile,
           method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case 0xBF => new OpAThrow(reader, cf, method, lineNo, code)
    }
  }
}

class OpAThrow(val reader: StreamReader,
               override val cf: ClassFile,
               override val method: MethodInfo,
               val lineNo: Int,
               val opCode: Int,
              ) extends Op {
  override val opName = {
    "athrow"
  }

  override def proc(ctx: RtCtx): Unit = ???
}
