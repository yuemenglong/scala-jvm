package io.github.yuemenglong.jvm.nativ

import io.github.yuemenglong.json.JSON
import io.github.yuemenglong.jvm.struct.ClassFile

/**
  * Created by <yuemenglong@126.com> on 2018/2/22.
  */
class Arr(val ty: Any, val count: Int) extends Ref {
  val array: Array[Any] = new Array(count)

  val tyName = ty match {
    case s: String => s
    case cf: ClassFile => cf.name
  }

  override def toString = s"Arr[${tyName}]@[${id}]"
}
