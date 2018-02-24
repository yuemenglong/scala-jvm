package io.github.yuemenglong.jvm

import io.github.yuemenglong.jvm.rt.Vm
import io.github.yuemenglong.jvm.struct.CpInfo

object App {
  def main(args: Array[String]): Unit = {
    Vm.rt.clazzpath("target/test-classes")
    Vm.rt.clazzpath(s"${sys.env("JAVA_HOME")}/jre/lib/rt.jar")
    //    Vm.init()
    val lib = Vm.rt.load("io/github/yuemenglong/jvm/Java")
    Vm.run(lib.main())
  }
}






