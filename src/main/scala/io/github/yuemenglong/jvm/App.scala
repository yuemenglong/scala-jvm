package io.github.yuemenglong.jvm

import io.github.yuemenglong.jvm.rt.Vm

object App {
  def main(args: Array[String]): Unit = {
    //    Vm.rt.load("target/classes/io/github/yuemenglong/jvm/Java.class")
    //    Vm.rt.load("C:/Program Files/Java/jdk1.8.0_131/jre/lib/rt.jar")
    //    val a = Vm.rt.clazzMap("java/lang/Object")
    val p = "java/lang/Object.class"
    Vm.rt.load(s"D:/rt/${p}")
    val a = Vm.rt.clazzMap("java/lang/Object")
    println(a)
  }
}






