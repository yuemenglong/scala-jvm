package io.github.yuemenglong.jvm.common

import java.lang.reflect.{Field, Method}

import io.github.yuemenglong.jvm.rt.{ThreadCtx, Vm}
import io.github.yuemenglong.jvm.struct.{ClassFile, FieldInfo, MethodInfo}

/**
  * Created by <yuemenglong@126.com> on 2018/2/19.
  */
object Kit {
  def makeVariableTable(ctx: ThreadCtx, n: Int): Map[Int, Any] = {
    (1 to n).map(_ => ctx.pop()).reverse.foldLeft(Map[Int, Any]()) { case (map, v) =>
      map.keys.isEmpty match {
        case true => map + (0 -> v)
        case false =>
          val max = map.keys.max
          map(max).isInstanceOf[Double] || map(max).isInstanceOf[Long] match {
            case true => map + (max + 2 -> v)
            case false => map + (max + 1 -> v)
          }
      }
    }
  }

  def debug(s: Any*): Unit = {
    println(s"${s.mkString(", ")}")
  }

  def findMethod(cf: ClassFile, name: String, descriptor: String): MethodInfo = {
    var cur = cf
    Stream.continually({
      val ret = cur.method(name, descriptor)
      if (ret == null) {
        cur = Vm.rt.superClazz(cur)
      }
      ret
    }).find(_ != null) match {
      case Some(m) => m
      case None => null
    }
  }

  def getCfFields(cf: ClassFile): Array[FieldInfo] = {
    val p = cf.sup match {
      case null => Array[FieldInfo]()
      case _ => getCfFields(Vm.rt.load(cf.sup))
    }
    cf.fields ++ p
  }

  def defaultFieldValue(f: FieldInfo): Any = {
    f.descriptor match {
      case "B" => 0.toByte
      case "C" => 0.toChar
      case "D" => 0.toDouble
      case "F" => 0.toFloat
      case "I" => 0
      case "J" => 0.toLong
      case "S" => 0.toShort
      case "Z" => false
      case _ => null
    }
  }

  def getDeclaredFields(clazz: Class[_]): Array[Field] = {
    val parent = clazz.getSuperclass
    if (parent != null) {
      getDeclaredFields(parent) ++ clazz.getDeclaredFields
    } else {
      clazz.getDeclaredFields
    }
  }

  def getDeclaredMethods(clazz: Class[_]): Array[Method] = {
    val parent = clazz.getSuperclass
    if (parent != null) {
      getDeclaredMethods(parent) ++ clazz.getDeclaredMethods
    } else {
      clazz.getDeclaredMethods
    }
  }
}
