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
      case 0xAC => new OpReturn[Int](reader, cf, method, lineNo, code)
      case 0xAD => new OpReturn[Long](reader, cf, method, lineNo, code)
      case 0xAE => new OpReturn[Float](reader, cf, method, lineNo, code)
      case 0xAF => new OpReturn[Double](reader, cf, method, lineNo, code)
      case 0xB0 => new OpReturn[AnyRef](reader, cf, method, lineNo, code)
      case 0xB1 => new OpReturn[Unit](reader, cf, method, lineNo, code)
    }
  }
}

class OpReturn[T: ClassTag](val reader: StreamReader,
                            override val cf: ClassFile,
                            override val method: MethodInfo,
                            val lineNo: Int,
                            val opCode: Int,
                           ) extends Op {
  override val opName = {
    val s = classTag[T].runtimeClass match {
      case Types.classOfInt => "i"
      case Types.classOfLong => "l"
      case Types.classOfFloat => "f"
      case Types.classOfDouble => "d"
      case Types.classOfRef => "a"
      case Types.classOfVoid => ""
    }
    s"${s}return"
  }

  override def proc(ctx: ThreadCtx): Unit = ???
}
