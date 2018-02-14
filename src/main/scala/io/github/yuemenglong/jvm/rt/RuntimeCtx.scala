package io.github.yuemenglong.jvm.rt

import java.io.{File, FileInputStream}

import io.github.yuemenglong.jvm.common.StreamReader
import io.github.yuemenglong.jvm.op.Op
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.interpreter.InputStream

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */

class RuntimeCtx {
  var heap: Any = _
  var clazzMap: Map[String, ClassFile] = Map()
  var threads: ArrayBuffer[ThreadCtx] = new ArrayBuffer[ThreadCtx]()

  private def read(is: InputStream): Seq[Byte] = {
    val buffer = new Array[Byte](4096)
    Stream.continually({
      val len = is.read(buffer)
      len match {
        case -1 => null
        case _ => buffer.take(len)
      }
    }).takeWhile(_ != null).flatten
  }

  def createThread(method: MethodInfo): ThreadCtx = {
    threads += new ThreadCtx(method, this)
    threads.last
  }

  def load(path: String): Unit = load(new File(path))

  def load(file: File): Unit = {
    if (file.isDirectory) {
      file.listFiles().foreach(load)
    } else if (file.getName.endsWith(".class")) {
      val reader = new StreamReader(read(new FileInputStream(file)))
      val cf = new ClassFile(reader)
      clazzMap += (cf.name -> cf)
    }
  }
}






