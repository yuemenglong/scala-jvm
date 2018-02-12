package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.reflect.{ClassTag, classTag}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpNew {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, code: Int): Op = {
    code match {
      case 0xBB => new OpNew(reader, cf, method, code)
      case 0xBC => new OpNewArray(reader, cf, method, code)
      case 0xBD => new OpANewArray(reader, cf, method, code)
    }
  }
}

class OpNew(val reader: StreamReader,
            override val cf: ClassFile,
            override val method: MethodInfo,
            val opCode: Int,
           ) extends Op {
  val index: Short = reader.readShort()
  override val opName = {
    s"new ${cp(index)}"
  }

  override def proc(ctx: RtCtx): Unit = ???
}

class OpNewArray(val reader: StreamReader,
                 override val cf: ClassFile,
                 override val method: MethodInfo,
                 val opCode: Int,
                ) extends Op {
  val atype: Short = reader.readByte()

  def ty: String = {
    atype match {
      case 4 => "T_BOOLEAN"
      case 5 => "T_CHAR"
      case 6 => "T_FLOAT"
      case 7 => "T_DOUBLE"
      case 8 => "T_BYTE"
      case 9 => "T_SHORT"
      case 10 => "T_INT"
      case 11 => "T_LONG"
    }
  }

  override val opName = {
    s"newarray ${ty}"
  }

  override def proc(ctx: RtCtx): Unit = ???
}

class OpANewArray(val reader: StreamReader,
                  override val cf: ClassFile,
                  override val method: MethodInfo,
                  val opCode: Int,
                 ) extends Op {
  val index: Short = reader.readShort()

  override val opName = {
    s"anewarray ${cp(index)}"
  }

  override def proc(ctx: RtCtx): Unit = ???
}
