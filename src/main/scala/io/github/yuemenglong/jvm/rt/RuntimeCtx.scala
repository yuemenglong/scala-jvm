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
  private var counter: Long = 0
  private var heap: Map[Long, Obj] = Map()
  private var clazzLoaderMap: Map[String, InputStream] = Map()
  private var clazzMap: Map[String, ClassFile] = Map()
  private var threads: ArrayBuffer[ThreadCtx] = new ArrayBuffer[ThreadCtx]()

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

  def createObject(cf: ClassFile): Obj = {
    counter += 1
    val obj = new Obj(cf, counter)
    heap += (obj.id -> obj)
    obj
  }

  def createThread(method: MethodInfo): ThreadCtx = {
    if (!clazzMap.contains(method.cf.name)) {
      clazzMap += (method.cf.name -> method.cf)
    }
    threads += new ThreadCtx(method, this)
    threads.last
  }

  def superClazz(cf: ClassFile): ClassFile = {
    cf.sup match {
      case null => null
      case _ => load(cf.sup)
    }
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






