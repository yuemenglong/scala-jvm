package io.github.yuemenglong.jvm.rt

import io.github.yuemenglong.jvm.common.Kit
import io.github.yuemenglong.jvm.struct.MethodInfo

/**
  * Created by <yuemenglong@126.com> on 2018/2/13.
  */
object Vm {

  val rt: RuntimeCtx = new RuntimeCtx

  def init(): Unit = {
    // 调用initializeSystemClass
    val m = rt.load("java/lang/System").method("initializeSystemClass")
    run(m)
    Kit.debug("Finish Init")
  }

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
