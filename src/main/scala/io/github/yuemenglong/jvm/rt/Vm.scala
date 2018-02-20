package io.github.yuemenglong.jvm.rt

import io.github.yuemenglong.jvm.common.Kit
import io.github.yuemenglong.jvm.struct.MethodInfo

/**
  * Created by <yuemenglong@126.com> on 2018/2/13.
  */
object Vm {

  val rt: RuntimeCtx = new RuntimeCtx

  def run(ctx: ThreadCtx): Any = {
    def isFinish: Boolean = ctx.frames.isEmpty

    while (!isFinish) {
      Kit.debug(ctx)
      val code = ctx.code()
      Kit.debug(f"[${code.cf.simpleName}:${code.method.name}] [${code.lineNo}] ${code}")
      ctx.inc()
      code.proc(ctx)
    }
    rt.finishThread(ctx)
  }

  def run(method: MethodInfo): Any = {
    run(rt.createThread(method))
  }
}
