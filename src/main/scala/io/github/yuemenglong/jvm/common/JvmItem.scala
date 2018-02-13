package io.github.yuemenglong.jvm.common

import io.github.yuemenglong.json.lang.JsonIgnore
import io.github.yuemenglong.jvm.struct.{ClassFile, CpInfo, ValuedCpInfo}

/**
  * Created by <yuemenglong@126.com> on 2018/2/11.
  */
@JsonIgnore(Array("reader", "cf", "method"))
trait JvmItem {
  val cf: ClassFile

  def cp(idx: Int): CpInfo = cf.constant_pool(idx)

  def cpv(idx: Int) = cp(idx).asInstanceOf[ValuedCpInfo]
}
