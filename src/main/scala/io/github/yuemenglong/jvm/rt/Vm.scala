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
      val code = ctx.frame.code(ctx.pc)
      ctx.inc()
      println(ctx)
      println(s"${code}")
      code.proc(ctx)
    }
    println("Finish")
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
