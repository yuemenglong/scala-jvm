package io.github.yuemenglong.jvm.rt

import io.github.yuemenglong.jvm.struct.MethodInfo

/**
  * Created by <yuemenglong@126.com> on 2018/2/13.
  */
object Vm {
  def run(ctx: ThreadCtx): Any = {
    def isFinish: Boolean = ctx.frames.isEmpty

    while (!isFinish) {
      val code = ctx.frame.code(ctx.pc)
      ctx.inc()
      println(ctx.frame)
      println(s"${code}")
      code.proc(ctx)
    }
  }

  def run(method: MethodInfo): Any = {
    val runtimeCtx = new RuntimeCtx
    run(runtimeCtx.newThread(method))
  }
}
