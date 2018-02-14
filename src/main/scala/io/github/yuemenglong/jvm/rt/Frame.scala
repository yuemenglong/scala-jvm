package io.github.yuemenglong.jvm.rt

import io.github.yuemenglong.jvm.struct.MethodInfo

/**
  * Created by <yuemenglong@126.com> on 2018/2/14.
  */
class Frame(val method: MethodInfo, map: Map[Int, Any] = Map()) {
  var pc: Int = 0
  var localVariable: Map[Int, Any] = map

  def get(idx: Int): Any = localVariable(idx)

  def set(idx: Int, value: Any): Unit = localVariable += (idx -> value)

  def code(pc: Int) = method.code.code(pc)


}
