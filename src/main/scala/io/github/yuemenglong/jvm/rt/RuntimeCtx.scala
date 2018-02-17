package io.github.yuemenglong.jvm.rt

import java.io.{File, FileInputStream}
import java.nio.file.Paths

import io.github.yuemenglong.jvm.common.StreamReader
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.interpreter.InputStream
import java.util.jar.JarFile

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */

class RuntimeCtx {
  var heap: Any = _
  var clazzLoaderMap: Map[String, InputStream] = Map()
  var clazzMap: Map[String, ClassFile] = Map()
  var threads: ArrayBuffer[ThreadCtx] = new ArrayBuffer[ThreadCtx]()

  def clazzpath(root: String): Unit = {
    if (root.endsWith(".jar")) {
      val jf = new JarFile(root)
      val es = jf.entries()
      Stream.continually({
        es.hasMoreElements match {
          case true => es.nextElement()
          case false => null
        }
      }).takeWhile(_ != null).foreach(je => {
        if (!je.isDirectory && je.getName.endsWith(".class")) {
          clazzLoaderMap += (je.getName.replace(".class", "") -> jf.getInputStream(je))
        }
      })
    } else {
      val rootAbs = Paths.get(root).toAbsolutePath

      def go(file: File): Unit = {
        if (file.isDirectory) {
          file.listFiles().foreach(go)
        } else if (file.getName.endsWith(".class")) {
          val rel = rootAbs.relativize(Paths.get(file.getAbsolutePath)).toString.replaceAll("\\\\", "/")
          clazzLoaderMap += (rel.replace(".class", "") -> new FileInputStream(file))
        }
      }

      go(new File(root))
    }
  }

  def createThread(method: MethodInfo): ThreadCtx = {
    threads += new ThreadCtx(method, this)
    threads.last
  }

  def load(path: String): ClassFile = {
    if (clazzMap.contains(path)) {
      clazzMap(path)
    } else if (clazzLoaderMap.contains(path)) {
      load(clazzLoaderMap(path))
    } else
      throw new RuntimeException(s"Unknown Class [${path}]")
  }

  def load(is: InputStream): ClassFile = {
    val reader = new StreamReader(is)
    val cf = new ClassFile(reader)
    clazzMap += (cf.name -> cf)
    println(s"[Load] ${cf.name}")
    cf
  }
}






