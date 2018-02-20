package io.github.yuemenglong.jvm.rt

import io.github.yuemenglong.jvm.struct.ClassFile

/**
  * Created by <yuemenglong@126.com> on 2018/2/20.
  */
class RtClazz(cf: ClassFile) {
  var map: Map[String, Any] = cf.fields.filter(f => {
    f.accessFlags.contains("ACC_STATIC")
  }).map(f => (f.name, null)).toMap

  def putStatic(key: String, value: Any): Unit = map += (key -> value)

  def getStatic(key: String): Any = map(key)
}
