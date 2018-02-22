package io.github.yuemenglong.jvm.nativ

import io.github.yuemenglong.json.JSON
import io.github.yuemenglong.jvm.struct.ClassFile

/**
  * Created by <yuemenglong@126.com> on 2018/2/19.
  */

class Obj(val cf: ClassFile) extends Ref {
  private[nativ] var fields: Map[String, Any] = cf.fields.map(f => {
    (f.name, null)
  }).toMap

  def get(key: String): Any = fields(key)

  def set(key: String, value: Any): Unit = fields += (key -> value)

  override def toString: String = {
    s"[${cf.simpleName}]@[${id}]"
  }
}
