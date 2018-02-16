package io.github.yuemenglong.jvm.rt

import java.io.{File, FileInputStream}

import io.github.yuemenglong.jvm.common.StreamReader
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.interpreter.InputStream
import java.util.jar.JarEntry
import java.util.jar.JarFile

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
      load(new FileInputStream(file))
    } else if (file.getName.endsWith(".jar")) {
      val jf = new JarFile(file)
      val es = jf.entries()
      Stream.continually({
        es.hasMoreElements match {
          case true => es.nextElement()
          case false => null
        }
      }).takeWhile(_ != null).foreach(je => {
        if (!je.isDirectory && je.getName.endsWith(".class")) {
          load(jf.getInputStream(je))
        }
      })
    }
  }

  def load(is: InputStream): Unit = {
    val reader = new StreamReader(is)
    val cf = new ClassFile(reader)
    clazzMap += (cf.name -> cf)
    println(s"[Load] ${cf.name}")
  }
}






