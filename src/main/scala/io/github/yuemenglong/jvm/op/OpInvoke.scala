package io.github.yuemenglong.jvm.op

import io.github.yuemenglong.jvm.common.StreamReader
import io.github.yuemenglong.jvm.rt.ThreadCtx
import io.github.yuemenglong.jvm.struct.{ClassFile, ConstantMethodrefInfo, MethodInfo}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpInvoke {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case 0xB6 => new OpInvokeVirtual(reader, cf, method, lineNo, code)
      case 0xB7 => new OpInvokeSpecial(reader, cf, method, lineNo, code)
      case 0xB8 => new OpInvokeStatic(reader, cf, method, lineNo, code)
      case 0xB9 => new OpInvokeInterface(reader, cf, method, lineNo, code)
      case 0xBA => new OpInvokeDynamic(reader, cf, method, lineNo, code)
    }
  }
}

class OpInvokeDynamic(val reader: StreamReader,
                      override val cf: ClassFile,
                      override val method: MethodInfo,
                      val lineNo: Int,
                      val opCode: Int,
                     ) extends Op {
  val index: Short = reader.readShort()
  val p: Short = reader.readShort()
  require(p == 0)

  override val opName = {
    s"invokedynamic ${cf.constant_pool(index)}"
  }

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpInvokeInterface(val reader: StreamReader,
                        override val cf: ClassFile,
                        override val method: MethodInfo,
                        val lineNo: Int,
                        val opCode: Int,
                       ) extends Op {
  val index: Short = reader.readShort()
  val count: Byte = reader.readByte()
  val p: Byte = reader.readByte()
  require(count > 0 && p == 0)

  override val opName = {
    s"invokeinterface ${cf.constant_pool(index)}"
  }

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpInvokeSpecial(val reader: StreamReader,
                      override val cf: ClassFile,
                      override val method: MethodInfo,
                      val lineNo: Int,
                      val opCode: Int,
                     ) extends Op {
  val index: Short = reader.readShort()
  override val opName = {
    s"invokespecial ${cf.constant_pool(index)}"
  }

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpInvokeStatic(val reader: StreamReader,
                     override val cf: ClassFile,
                     override val method: MethodInfo,
                     val lineNo: Int,
                     val opCode: Int,
                    ) extends Op {
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

class OpInvokeVirtual(val reader: StreamReader,
                      override val cf: ClassFile,
                      override val method: MethodInfo,
                      val lineNo: Int,
                      val opCode: Int,
                     ) extends Op {
  val index: Short = reader.readShort()
  override val opName = {
    s"invokevirtual ${cf.constant_pool(index)}"
  }

  override def proc(ctx: ThreadCtx): Unit = ???
}