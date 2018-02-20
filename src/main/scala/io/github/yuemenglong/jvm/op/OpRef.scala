package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.{Kit, StreamReader}
import io.github.yuemenglong.jvm.rt.{ThreadCtx, Vm}
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
  val index: Short = reader.readShort()
  val prefix: String = (opCode - 0xB2) % 2 match {
    case 0 => "get"
    case 1 => "put"
  }
  val postfix: String = (opCode - 0xB2) / 2 match {
    case 0 => "static"
    case 1 => "field"
  }

  override val opName = s"${prefix}${postfix} ${cp(index)}"

  override def proc(ctx: ThreadCtx): Unit = {
    s"${prefix}${postfix}" match {
      case "getstatic" =>
        val info = ctx.rt.load(cpf(index).clazz).field(cpf(index).name, cpf(index).descriptor)
        val field = ctx.rt.getStatic(info.cf, info.name)
        ctx.push(field)
      case "putstatic" =>
        val info = ctx.rt.load(cpf(index).clazz).field(cpf(index).name, cpf(index).descriptor)
        val field = ctx.pop()
        ctx.rt.putStatic(info.cf, info.name, field)
      case _ => ???
    }
  }
}

object Invoke {

  class OpInvokeDynamic(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
    val index: Short = reader.readShort()
    val p: Short = reader.readShort()
    require(p == 0)

    override val opName = s"invokedynamic ${cf.constant_pool(index)}"

    override def proc(ctx: ThreadCtx): Unit = ???
  }

  class OpInvokeInterface(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
    val index: Short = reader.readShort()
    val count: Byte = reader.readByte()
    val p: Byte = reader.readByte()
    require(count > 0 && p == 0)

    override val opName = s"invokeinterface ${cf.constant_pool(index)}"

    override def proc(ctx: ThreadCtx): Unit = ???
  }

  class OpInvokeSpecial(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
    val index: Short = reader.readShort()
    override val opName = s"invokespecial ${cf.constant_pool(index)}"

    override def proc(ctx: ThreadCtx): Unit = {
      val info = cp(index)
      info match {
        case mr: ConstantMethodrefInfo =>
          val cur = ctx.rt.load(mr.clazz)
          val m = Kit.findMethod(cur, mr.name, mr.descriptor)
          val map = Kit.makeVariableTable(ctx, m.paramsType.length + 1)
          ctx.call(m, map)
        case _ => ???
      }
    }
  }

  class OpInvokeStatic(val reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
    val index: Short = reader.readShort()
    override val opName = s"invokestatic ${cf.constant_pool(index)}"

    override def proc(ctx: ThreadCtx): Unit = {
      val ref = cp(index).asInstanceOf[ConstantMethodrefInfo]
      val method = ctx.rt.load(ref.clazz).method(ref.name, ref.descriptor)
      if (method.accessFlags.contains("ACC_NATIVE")) {
        ctx.rt.callStatic(method.cf, method.name, method.descriptor)()
      } else {
        val map = Kit.makeVariableTable(ctx, method.paramsType.length)
        ctx.call(method, map)
      }
    }
  }

  class OpInvokeVirtual(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
    val index: Short = reader.readShort()
    override val opName = s"invokevirtual ${cf.constant_pool(index)}"

    override def proc(ctx: ThreadCtx): Unit = {
      val info = cp(index).asInstanceOf[ConstantMethodrefInfo]
      ???
    }
  }

}

object New {

  class OpNew(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
    val index: Short = reader.readShort()
    override val opName = s"new ${cp(index)}"

    override def proc(ctx: ThreadCtx): Unit = {
      val cf = ctx.rt.load(cpc(index).name)
      val obj = ctx.rt.createObject(cf)
      ctx.push(obj)
    }
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

    override val opName = s"newarray ${ty}"

    override def proc(ctx: ThreadCtx): Unit = ???
  }

  class OpANewArray(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
    val index: Short = reader.readShort()

    override val opName = s"anewarray ${cp(index)}"

    override def proc(ctx: ThreadCtx): Unit = ???
  }

}

class OpArrayLength(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  override val opName = "arraylength"

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpAThrow(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  override val opName = "athrow"

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpCheck(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val index: Short = reader.readShort()
  override val opName: String = {
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
  override val opName: String = opCode match {
    case 0xC2 => "monitorenter"
    case 0xC3 => "monitorexit"
  }

  override def proc(ctx: ThreadCtx): Unit = ???
}