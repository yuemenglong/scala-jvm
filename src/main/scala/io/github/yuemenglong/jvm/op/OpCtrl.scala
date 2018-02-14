package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.jvm.rt.ThreadCtx
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
      case 0xA8 => new OpJsr(reader, cf, method, lineNo, code)
      case 0xA9 => new OpRet(reader, cf, method, lineNo, code)
      case 0xAA => new OpTableSwitch(reader, cf, method, lineNo, code)
      case 0xAB => new OpLookupSwitch(reader, cf, method, lineNo, code)
      case c if 0xAC <= c && c <= 0xB1 => new OpReturn(reader, cf, method, lineNo, code)
    }
  }
}

class OpGoto(val reader: StreamReader,
             override val cf: ClassFile,
             override val method: MethodInfo,
             val lineNo: Int,
             val opCode: Int,
            ) extends Op {
  val offset: Short = reader.readShort()
  override val opName = {
    s"goto ${offset}"
  }

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpJsr(val reader: StreamReader,
            override val cf: ClassFile,
            override val method: MethodInfo,
            val lineNo: Int,
            val opCode: Int,
           ) extends Op {
  val offset: Short = reader.readShort()
  override val opName = {
    s"jsr ${offset}"
  }

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpRet(val reader: StreamReader,
            override val cf: ClassFile,
            override val method: MethodInfo,
            val lineNo: Int,
            val opCode: Int,
           ) extends Op {
  val varnum: Byte = reader.readByte()
  override val opName = {
    s"ret ${varnum}"
  }

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpTableSwitch(val reader: StreamReader,
                    override val cf: ClassFile,
                    override val method: MethodInfo,
                    val lineNo: Int,
                    val opCode: Int,
                   ) extends Op {
  val pad: Array[Byte] = reader.readBytes(3)
  val dft: Int = reader.readInt()
  val low: Int = reader.readInt()
  val high: Int = reader.readInt()
  val offsets: Array[Int] = (1 to high - low + 1).map(_ => reader.readInt()).toArray
  override val opName = {
    s"tableswitch [${dft}|${low}|${high}] [${offsets.mkString(",")}]"
  }

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpLookupSwitch(val reader: StreamReader,
                     override val cf: ClassFile,
                     override val method: MethodInfo,
                     val lineNo: Int,
                     val opCode: Int,
                    ) extends Op {
  val pad = reader.readBytes(3)
  val dft = reader.readInt()
  val npairs = reader.readInt()
  val offset_pairs = (1 to npairs).map(_ => (reader.readInt(), reader.readInt()))
  override val opName = {
    s"lookupswitch [${dft}|${npairs}] [${offset_pairs.map(p => s"${p._1}->${p._2}").mkString(",")}]"
  }

  override def proc(ctx: ThreadCtx): Unit = ???
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