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
    Vm.rt.load("target/classes/io/github/yuemenglong/jvm/Java.class")
    Vm.run()
  }
}






