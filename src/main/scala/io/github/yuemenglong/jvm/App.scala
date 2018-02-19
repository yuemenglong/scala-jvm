package io.github.yuemenglong.jvm

import io.github.yuemenglong.jvm.rt.Vm

object App {
  def main(args: Array[String]): Unit = {
    Vm.rt.clazzpath("target/test-classes")
    Vm.rt.clazzpath("C:/Program Files/Java/jdk1.8.0_131/jre/lib/rt.jar")
    val a = Vm.rt.load("io/github/yuemenglong/jvm/Java")
    println(a.todo())
  }
}






