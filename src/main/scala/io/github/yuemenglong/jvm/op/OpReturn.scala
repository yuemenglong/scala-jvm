package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{ClassFile, StreamReader}
import io.github.yuemenglong.jvm.struct.MethodInfo

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpReturn {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, code: Byte): OpReturn[_] = {
    code match {
      case 0xAC => new OpReturn[Int](reader, cf, method, code)
      case 0xAD => new OpReturn[Long](reader, cf, method, code)
      case 0xAE => new OpReturn[Float](reader, cf, method, code)
      case 0xAF => new OpReturn[Double](reader, cf, method, code)
      case 0xB0 => new OpReturn[AnyRef](reader, cf, method, code)
      case 0xB1 => new OpReturn[Unit](reader, cf, method, code)
    }
  }
}

class OpReturn[T](val reader: StreamReader,
                  override val cf: ClassFile,
                  override val method: MethodInfo,
                  val opCode: Byte,
                 ) extends Op {
  require(n >= 0 && n <= 3)

  private def n = opCode - 0x2A

  private def idx = n + 1

  override val opName = {
    val s = opCode match {
      case 0xAC => "i"
      case 0xAD => "l"
      case 0xAE => "f"
      case 0xAF => "d"
      case 0xD0 => "a"
      case 0xD1 => ""
    }
    s"${s}return"
  }

  override def proc(ctx: RtCtx): Unit = {

  }
}
