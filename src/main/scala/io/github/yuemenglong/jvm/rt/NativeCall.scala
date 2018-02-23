package io.github.yuemenglong.jvm.rt

import io.github.yuemenglong.jvm.nativ.{Obj, Ref, Str}

/**
  * Created by <yuemenglong@126.com> on 2018/2/22.
  */
object NativeCall {
  type NativeFn = (ThreadCtx, Map[Int, Any]) => Unit

  val staticNatives: Map[(String, String, String), NativeFn] = Map(
    ("java/lang/Object", "registerNatives", "()V") -> ((_, _) => {}),
    ("java/lang/System", "registerNatives", "()V") -> ((_, _) => {}),
    ("java/lang/Class", "registerNatives", "()V") -> ((_, _) => {}),
    ("java/lang/System", "arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V") -> ((_, _) => {
      ???
    }),
    ("java/lang/Class", "getPrimitiveClass", "(Ljava/lang/String;)Ljava/lang/Class;") -> ((ctx, vt) => {
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
  )
  val virtualNatives: Map[(String, String, String), NativeFn] = Map(
    ("java/lang/Class", "isInterface", "()Z") -> ((ctx, vt) => {
      ???
      //      ctx.push(0)
    }),
    ("java/lang/Class", "isPrimitive", "()Z") -> ((ctx, vt) => {
      ???
      //      require(!vt(0).isInstanceOf[Ref])
      //      ctx.push(0)
    }),
    ("java/lang/Object", "getClass", "()Ljava/lang/Class;") -> ((ctx, vt) => {
      val obj = ctx.pop().asInstanceOf[Obj]
      ctx.push(Vm.rt.getClass(obj.cf))
    }),
  )
}
