package io.github.yuemenglong.jvm.nativ

import io.github.yuemenglong.jvm.rt.ThreadCtx
import io.github.yuemenglong.jvm.struct.ClassFile

/**
  * Created by <yuemenglong@126.com> on 2018/2/19.
  */
object Obj {
  private var counter: Long = 0

  def inc(): Long = {
    counter += 1
    counter
  }
}

class Obj(val cf: ClassFile) {
  val id: Long = Obj.inc()
  var monitor: Int = 0
  var tid: Long = 0
  private var fields: Map[String, Any] = cf.fields.map(f => {
    (f.name, null)
  }).toMap

  def get(key: String): Any = fields(key)

  def set(key: String, value: Any): Unit = fields += (key -> value)

  def monitorEnter(ctx: ThreadCtx): Boolean = {
    monitor match {
      case _ if monitor > 0 => tid match {
        case ctx.id =>
          monitor += 1
          true
        case _ => false
      }
      case 0 =>
        tid = ctx.id
        monitor += 1
        true
    }
  }

  def monitorExit(ctx: ThreadCtx) = {
    monitor -= 1
    if (monitor == 0) {
      tid = 0
    }
  }

  override def toString: String = {
    s"[${cf.simpleName}]@[${id}]"
  }
}
