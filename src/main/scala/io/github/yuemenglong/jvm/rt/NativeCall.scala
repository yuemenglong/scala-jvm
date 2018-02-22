package io.github.yuemenglong.jvm.rt

/**
  * Created by <yuemenglong@126.com> on 2018/2/22.
  */
object NativeCall {
  type NativeFn = (ThreadCtx, Map[Int, Any]) => Unit

  val staticNatives: Map[(String, String, String), NativeFn] = Map(
    ("java/lang/Object", "registerNatives", "()V") -> ((_, _) => {}),
    ("java/lang/System", "registerNatives", "()V") -> ((_, _) => {}),
    ("java/lang/Class", "registerNatives", "()V") -> ((_, _) => {}),
  )
  val virtualNatives: Map[(String, String, String), NativeFn] = Map(
    ("java/lang/Class", "isInterface", "()Z") -> ((ctx, vt) => {
      ctx.push(0)
    }),
    ("java/lang/Class", "isPrimitive", "()Z") -> ((ctx, vt) => {
      ctx.push(0)
    }),
  )
}
