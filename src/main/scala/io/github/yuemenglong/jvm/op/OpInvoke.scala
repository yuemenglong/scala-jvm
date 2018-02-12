package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.reflect.{ClassTag, classTag}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpInvoke {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, code: Int): Op = {
    code match {
      case 0xB7 => new OpInvokeSpecial(reader, cf, method, code)
    }
  }
}

class OpInvokeSpecial(val reader: StreamReader,
                      override val cf: ClassFile,
                      override val method: MethodInfo,
                      val opCode: Int,
                     ) extends Op {
  val index: Short = reader.readShort()
  override val opName = {
    s"invokespecial ${cf.constant_pool(index)}"
  }

  override def proc(ctx: RtCtx): Unit = ???
}
