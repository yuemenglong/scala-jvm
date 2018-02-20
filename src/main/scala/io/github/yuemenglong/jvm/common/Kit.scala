package io.github.yuemenglong.jvm.common

import io.github.yuemenglong.jvm.rt.{ThreadCtx, Vm}
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

/**
  * Created by <yuemenglong@126.com> on 2018/2/19.
  */
object Kit {
  def makeVariableTable(ctx: ThreadCtx, n: Int): Map[Int, Any] = {
    val map = (1 to n).map(_ => ctx.pop()).reverse.foldLeft(Map[Int, Any]()) { case (map, v) =>
      map.keys.isEmpty match {
        case true => map + (0 -> v)
        case false =>
          val max = map.keys.max
          map(max).isInstanceOf[Double] || map(max).isInstanceOf[Long] match {
            case true => map + (max + 2 -> v)
            case false => map + (max + 1 -> v)
          }
      }
    }
    map
  }

  def debug(s: Any*): Unit = {
    println(s"${s.mkString(", ")}")
  }

  def findMethod(cf: ClassFile, name: String, descriptor: String): MethodInfo = {
    var cur = cf
    Stream.continually({
      val ret = cur.method(name, descriptor)
      if (ret == null) {
        cur = Vm.rt.superClazz(cur)
      }
      ret
    }).find(_ != null) match {
      case Some(m) => m
      case None => null
    }
  }
}
