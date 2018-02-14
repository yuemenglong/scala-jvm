package io.github.yuemenglong.jvm

import io.github.yuemenglong.jvm.rt.Vm

object App {
  def main(args: Array[String]): Unit = {
    Vm.rt.load("target/classes/io/github/yuemenglong/jvm/Java.class")
    //    Vm.rt.load("C:/Program Files/Java/jdk1.8.0_65/jre/lib/rt.jar")
    //    val p = "com/sun/activation/registries/MailcapTokenizer.class"
    //    Vm.rt.load(s"D:/${p}")
    //    Vm.run()
  }
}






