package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.StreamReader
import io.github.yuemenglong.jvm.rt.ThreadCtx
import io.github.yuemenglong.jvm.struct.{ClassFile, ConstantClassInfo, ConstantMethodrefInfo, MethodInfo}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpRef {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case c if 0xB2 <= c && c <= 0xB5 => new OpStatic(reader, cf, method, lineNo, code)
      case 0xB6 => new Invoke.OpInvokeVirtual(reader, cf, method, lineNo, code)
      case 0xB7 => new Invoke.OpInvokeSpecial(reader, cf, method, lineNo, code)
      case 0xB8 => new Invoke.OpInvokeStatic(reader, cf, method, lineNo, code)
      case 0xB9 => new Invoke.OpInvokeInterface(reader, cf, method, lineNo, code)
      case 0xBA => new Invoke.OpInvokeDynamic(reader, cf, method, lineNo, code)
      case 0xBB => new New.OpNew(reader, cf, method, lineNo, code)
      case 0xBC => new New.OpNewArray(reader, cf, method, lineNo, code)
      case 0xBD => new New.OpANewArray(reader, cf, method, lineNo, code)
      case 0xBE => new OpArrayLength(reader, cf, method, lineNo, code)
      case 0xBF => new OpAThrow(reader, cf, method, lineNo, code)
      case c if 0xC0 <= c && c <= 0xC1 => new OpCheck(reader, cf, method, lineNo, code)
      case c if 0xC2 <= c && c <= 0xC3 => new OpMonitor(reader, cf, method, lineNo, code)
    }
  }
}

class OpStatic(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val prefix = (opCode - 0xB2) % 2 match {
    case 0 => "get"
    case 1 => "put"
  }
  val postfix = (opCode - 0xB2) / 2 match {
    case 0 => "static"
    case 1 => "field"
  }

  override val opName = s"${prefix}${postfix}"

  override def proc(ctx: ThreadCtx): Unit = ???
}

object Invoke {

  class OpInvokeDynamic(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
    val index: Short = reader.readShort()
    val p: Short = reader.readShort()
    require(p == 0)

    override val opName = {
      s"invokedynamic ${cf.constant_pool(index)}"
    }

    override def proc(ctx: ThreadCtx): Unit = ???
  }

  class OpInvokeInterface(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
    val index: Short = reader.readShort()
    val count: Byte = reader.readByte()
    val p: Byte = reader.readByte()
    require(count > 0 && p == 0)

    override val opName = {
      s"invokeinterface ${cf.constant_pool(index)}"
    }

    override def proc(ctx: ThreadCtx): Unit = ???
  }

  class OpInvokeSpecial(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
    val index: Short = reader.readShort()
    override val opName = {
      s"invokespecial ${cf.constant_pool(index)}"
    }

    override def proc(ctx: ThreadCtx): Unit = ???
  }

  class OpInvokeStatic(val reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
    val index: Short = reader.readShort()
    override val opName = {
      s"invokestatic ${cf.constant_pool(index)}"
    }

    override def proc(ctx: ThreadCtx): Unit = {
      val ref = cp(index).asInstanceOf[ConstantMethodrefInfo]
      val method = ctx.rt.clazzMap(ref.clazz).method(ref.name, ref.descriptor)
      var idx = 0
      var map = Map[Int, Any]()
      method.paramsType.foreach(_ => {
        val p = ctx.pop()
        map += (idx -> p)
        idx += 1
        if (p.isInstanceOf[Double] || p.isInstanceOf[Long]) {
          //        map += (idx -> p)
          idx += 1
        }
      })
      ctx.call(method, map)
    }
  }

  class OpInvokeVirtual(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
    val index: Short = reader.readShort()
    override val opName = {
      s"invokevirtual ${cf.constant_pool(index)}"
    }

    override def proc(ctx: ThreadCtx): Unit = ???
  }

}

object New {

  class OpNew(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
    val index: Short = reader.readShort()
    override val opName = {
      s"new ${cp(index)}"
    }

    override def proc(ctx: ThreadCtx): Unit = ???
  }

  class OpNewArray(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
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

    override def proc(ctx: ThreadCtx): Unit = ???
  }

  class OpANewArray(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
    val index: Short = reader.readShort()

    override val opName = {
      s"anewarray ${cp(index)}"
    }

    override def proc(ctx: ThreadCtx): Unit = ???
  }

}

class OpArrayLength(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  override val opName = "arraylength"

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpAThrow(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  override val opName = {
    "athrow"
  }

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpCheck(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val index: Short = reader.readShort()
  override val opName = {
    val name = opCode match {
      case 0xC0 => "checkcast"
      case 0xC1 => "instanceof"
    }
    val clazzName = cp(index).asInstanceOf[ConstantClassInfo].name
    s"${name} ${clazzName}"
  }

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpMonitor(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  override val opName = opCode match {
    case 0xC2 => "monitorenter"
    case 0xC3 => "monitorexit"
  }

  override def proc(ctx: ThreadCtx): Unit = ???
}