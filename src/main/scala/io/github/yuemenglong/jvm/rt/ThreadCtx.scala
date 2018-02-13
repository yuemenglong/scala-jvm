package io.github.yuemenglong.jvm.rt

import io.github.yuemenglong.jvm.op.Op
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */

class RunTimeCtx {
  var heap: Any = _
  var methodArea: Map[String, ClassFile] = Map()
}

class ThreadCtx {
  var pc: Long = _
  var method: MethodInfo = _
  var rt: RunTimeCtx = _
  var stack: ArrayBuffer[Frame] = new ArrayBuffer[Frame]()
}

class Frame {
  var localVariable: ArrayBuffer[Any] = new ArrayBuffer[Any]()
  var stack: ArrayBuffer[Op] = new ArrayBuffer[Op]()
  var method: MethodInfo = _
}
