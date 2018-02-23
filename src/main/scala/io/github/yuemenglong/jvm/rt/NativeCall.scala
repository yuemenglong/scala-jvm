package io.github.yuemenglong.jvm.rt

import io.github.yuemenglong.jvm.nativ.{Obj, Ref, Str}

/**
  * Created by <yuemenglong@126.com> on 2018/2/22.
  */
object NativeCall {
  type NativeFn = (ThreadCtx) => Unit

  val staticNatives: Map[(String, String, String), NativeFn] = Map(
    ("java/lang/Object", "registerNatives", "()V") -> (_ => {}),
    ("java/lang/System", "registerNatives", "()V") -> (_ => {}),
    ("java/lang/Class", "registerNatives", "()V") -> (_ => {}),
    ("sun/misc/VM", "initialize", "()V") -> (_ => {}),
    ("java/lang/System", "initProperties", "(Ljava/util/Properties;)Ljava/util/Properties;") -> (_ => {}),
    ("java/lang/System", "arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V") -> (_ => {
      ???
    }),
    ("java/lang/Class", "getPrimitiveClass", "(Ljava/lang/String;)Ljava/lang/Class;") -> (ctx => {
      val name = ctx.pop().asInstanceOf[Str].inner
      val clazz = name match {
        case "char" => Vm.rt.getClass(Vm.rt.load("java/lang/Char"))
        case "byte" => Vm.rt.getClass(Vm.rt.load("java/lang/Byte"))
        case "short" => Vm.rt.getClass(Vm.rt.load("java/lang/Short"))
        case "int" => Vm.rt.getClass(Vm.rt.load("java/lang/Integer"))
        case "long" => Vm.rt.getClass(Vm.rt.load("java/lang/Long"))
        case "float" => Vm.rt.getClass(Vm.rt.load("java/lang/Float"))
        case "double" => Vm.rt.getClass(Vm.rt.load("java/lang/Double"))
      }
      ctx.push(clazz)
    }),
    ("java/lang/Class", "desiredAssertionStatus0", "(Ljava/lang/Class;)Z") -> (ctx => {
      ctx.pop()
      ctx.push(0)
    }),
    ("java/lang/Float", "floatToRawIntBits", "(F)I") -> (ctx => {
      val f = ctx.pop().asInstanceOf[Float]
      ctx.push(java.lang.Float.floatToRawIntBits(f))
    }),
    ("java/lang/Double", "doubleToRawLongBits", "(D)J") -> (ctx => {
      val d = ctx.pop().asInstanceOf[Double]
      ctx.push(java.lang.Double.doubleToRawLongBits(d))
    }),
    ("java/lang/Double", "longBitsToDouble", "(J)D") -> (ctx => {
      val v = ctx.pop().asInstanceOf[Long]
      ctx.push(java.lang.Double.longBitsToDouble(v))
    }),
  )
  val virtualNatives: Map[(String, String, String), NativeFn] = Map(
    ("java/lang/Class", "isInterface", "()Z") -> (ctx => {
      ???
      //      ctx.push(0)
    }),
    ("java/lang/Class", "isPrimitive", "()Z") -> (ctx => {
      ???
      //      require(!vt(0).isInstanceOf[Ref])
      //      ctx.push(0)
    }),
    ("java/lang/Object", "getClass", "()Ljava/lang/Class;") -> (ctx => {
      val obj = ctx.pop().asInstanceOf[Obj]
      ctx.push(Vm.rt.getClass(obj.cf))
    }),
  )
}
