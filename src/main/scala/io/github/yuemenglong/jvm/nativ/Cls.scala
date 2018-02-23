package io.github.yuemenglong.jvm.nativ

import io.github.yuemenglong.jvm.rt.Vm
import io.github.yuemenglong.jvm.struct.ClassFile

/**
  * Created by <yuemenglong@126.com> on 2018/2/23.
  */
class Cls(val typeCf: ClassFile) extends Obj(Vm.rt.load("java/lang/Class")) {

}
