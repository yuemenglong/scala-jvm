package io.github.yuemenglong.jvm

import java.io.FileInputStream

import io.github.yuemenglong.json.JSON
import io.github.yuemenglong.json.kit.Kit
import io.github.yuemenglong.json.lang.JsonIgnore
import io.github.yuemenglong.jvm.common.StreamReader
import io.github.yuemenglong.jvm.rt.Vm
import io.github.yuemenglong.jvm.struct.{ClassFile, CpInfo}

import scala.tools.nsc.interpreter.InputStream


object App {

  def read(is: InputStream): Seq[Byte] = {
    val buffer = new Array[Byte](4096)
    Stream.continually({
      val len = is.read(buffer)
      len match {
        case -1 => null
        case _ => buffer.take(len)
      }
    }).takeWhile(_ != null).flatten
  }

  def main(args: Array[String]): Unit = {
    //    val fs = new FileInputStream("target/classes/io/github/yuemenglong/jvm/Java.class")
    //    val fs = Thread.currentThread().getContextClassLoader.getResourceAsStream("Object.class")
    //    val stream = read(fs)
    //    val reader = new StreamReader(stream)
    //        println(stream.length)
    //        val a = stream.drop(stream.length - 1)
    //    val cf = new ClassFile(reader)
    //    CpInfo.debug(cf.constant_pool)
    //    println("===================================================================")
    //    println(cf)
    //    println("===================================================================")
    Vm.rt.load("target/classes/io/github/yuemenglong/jvm/Java.class")
    Vm.run()

    //    val m = cf.method("main")(0)
    //    println(m)
    //    println("===================================================================")
    //    Vm.run(m)
    //    println(JSON.pretty(cf.methods(1).attributes(0)))
  }
}






