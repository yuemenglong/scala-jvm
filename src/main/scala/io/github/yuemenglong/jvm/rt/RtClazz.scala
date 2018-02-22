package io.github.yuemenglong.jvm.rt

import io.github.yuemenglong.jvm.nativ.Obj
import io.github.yuemenglong.jvm.struct.ClassFile

/**
  * Created by <yuemenglong@126.com> on 2018/2/20.
  */
class RtClazz(val cf: ClassFile) {
  private var statics: Map[String, Any] = cf.fields.filter(f => {
    f.accessFlags.contains("ACC_STATIC")
  }).map(f => (f.name, null)).toMap

  val clazz: Obj = Vm.rt.createObject(Vm.rt.load("java/lang/Class"))

  def putStatic(key: String, value: Any): Unit = statics += (key -> value)

  def getStatic(key: String): Any = statics(key)
}
