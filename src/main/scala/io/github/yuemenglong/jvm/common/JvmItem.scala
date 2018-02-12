package io.github.yuemenglong.jvm.common

import io.github.yuemenglong.json.lang.JsonIgnore

/**
  * Created by <yuemenglong@126.com> on 2018/2/11.
  */
@JsonIgnore(Array("reader", "cf", "method"))
trait JvmItem {
  val cf: ClassFile
}
