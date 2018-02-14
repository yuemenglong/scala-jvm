package io.github.yuemenglong.jvm.rt

import io.github.yuemenglong.jvm.struct.MethodInfo

/**
  * Created by <yuemenglong@126.com> on 2018/2/13.
  */
object Vm {

  val rt: RuntimeCtx = new RuntimeCtx

  def run(ctx: ThreadCtx): Any = {
    def isFinish: Boolean = ctx.frames.isEmpty

    while (!isFinish) {
      println(ctx)
      val code = ctx.code()
      println(f"[${code.lineNo}] ${code}")
      ctx.inc()
      code.proc(ctx)
    }
  }

  def run(method: MethodInfo): Any = {
    rt.clazzMap += (method.cf.name -> method.cf)
    run(rt.createThread(method))
  }

  def run(): Any = {
    val method = rt.clazzMap.find(_._2.method("main").length > 0).get._2.method("main")(0)
    run(method)
  }
}
