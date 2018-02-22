package io.github.yuemenglong.jvm.rt

import java.io.{File, FileInputStream}
import java.nio.file.Paths

import io.github.yuemenglong.jvm.common.{Kit, StreamReader}
import io.github.yuemenglong.jvm.struct.{ClassFile, MethodInfo}

import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.interpreter.InputStream
import java.util.jar.JarFile

import io.github.yuemenglong.jvm.nativ.Obj

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */

class RuntimeCtx {
  private var heap: Map[Long, Obj] = Map()
  private var clazzLoaderMap: Map[String, InputStream] = Map()
  private var clazzMetaMap: Map[String, ClassFile] = Map()
  private var classMap: Map[String, RtClazz] = Map()
  private var threads: ArrayBuffer[ThreadCtx] = new ArrayBuffer[ThreadCtx]()

  private val staticNatives: Map[(String, String, String), (Map[Int, Any]) => Unit] = Map(
    ("java/lang/Object", "registerNatives", "()V") -> ((_) => {}),
    ("java/lang/System", "registerNatives", "()V") -> ((_) => {
      //初始化in,out,err等
      {
        val cf = load("java/io/PrintStream")
        val in = new Obj(cf)
        val out = new Obj(cf)
        val err = new Obj(cf)
        putStatic(cf, "in", in)
        putStatic(cf, "out", out)
        putStatic(cf, "err", err)
      }
    }),
  )

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
    val obj = new Obj(cf)
    heap += (obj.id -> obj)
    obj
  }

  def createThread(method: MethodInfo): ThreadCtx = {
    if (!clazzMetaMap.contains(method.cf.name)) {
      clazzMetaMap += (method.cf.name -> method.cf)
    }
    threads += new ThreadCtx(method, this)
    Kit.debug("[CreateThread]")
    threads.last
  }

  def finishThread(t: ThreadCtx): Unit = {
    Kit.debug("[FinishThread]")
    threads -= t
  }

  def superClazz(cf: ClassFile): ClassFile = {
    cf.sup match {
      case null => null
      case _ => load(cf.sup)
    }
  }

  def load(path: String): ClassFile = {
    if (clazzMetaMap.contains(path)) {
      clazzMetaMap(path)
    } else if (clazzLoaderMap.contains(path)) {
      load(clazzLoaderMap(path))
    } else
      throw new RuntimeException(s"Unknown Class [${path}]")
  }

  def load(is: InputStream): ClassFile = {
    val reader = new StreamReader(is)
    val cf = new ClassFile(reader)
    Kit.debug(s"[Load] ${cf.name}")
    classMap += (cf.name -> new RtClazz(cf))
    clazzMetaMap += (cf.name -> cf)
    val clinit = cf.method("<clinit>")
    if (clinit != null) {
      Vm.run(clinit)
    }
    Kit.debug(s"[Load] ${cf.name} SUCC")
    cf
  }

  def getStatic(cf: ClassFile, key: String): Any = classMap(cf.name).getStatic(key)

  def putStatic(cf: ClassFile, key: String, value: Any): Unit = classMap(cf.name).putStatic(key, value)

  def callStatic(cf: ClassFile, name: String, descriptor: String)(vt: Map[Int, Any] = Map()) = {
    val m = staticNatives(cf.name, name, descriptor)
    m(vt)
  }
}








