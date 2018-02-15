package io.github.yuemenglong.jvm.common

import io.github.yuemenglong.jvm.struct.{ClassFile, ConstantClassInfo, CpInfo, ValuedCpInfo}

/**
  * Created by <yuemenglong@126.com> on 2018/2/11.
  */
trait JvmItem {
  val cf: ClassFile

  def cp(idx: Any): CpInfo = {
    val i = idx match {
      case b: Byte => b match {
        case _ if b < 0 => b + 256
        case _ if b >= 0 => b
      }
      case s: Short => s match {
        case _ if s < 0 => s + 65536
        case _ if s >= 0 => s
      }
    }
    cf.constant_pool(i)
  }

  def cpv(idx: Any) = cp(idx).asInstanceOf[ValuedCpInfo]

  def cpc(idx: Any) = cp(idx).asInstanceOf[ConstantClassInfo]
}
