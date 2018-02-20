package io.github.yuemenglong.jvm.common

import io.github.yuemenglong.jvm.rt.ThreadCtx

/**
  * Created by <yuemenglong@126.com> on 2018/2/19.
  */
object Kit {
  def makeVariableTable(ctx: ThreadCtx, n: Int): Map[Int, Any] = {
    var idx = 0
    var map = Map[Int, Any]()
    (1 to n).foreach(_ => {
      val p = ctx.pop()
      map += (idx -> p)
      idx += 1
      if (p.isInstanceOf[Double] || p.isInstanceOf[Long]) {
        //        map += (idx -> p)
        idx += 1
      }
    })
    map
  }
}
