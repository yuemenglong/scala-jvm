package io.github.yuemenglong.jvm.struct

import io.github.yuemenglong.jvm.attribute.method.{CodeAttribute, SignatureAttribute}
import io.github.yuemenglong.jvm.common.{AccessFlagName, JvmItem, StreamReader}
import io.github.yuemenglong.jvm.op.Op

/**
  * Created by <yuemenglong@126.com> on 2018/2/11.
  */
class MethodInfo(reader: StreamReader, val cf: ClassFile) extends JvmItem with AccessFlagName {
  val access_flags: Short = reader.readShort()
  val name_index: Short = reader.readShort()
  val descriptor_index: Short = reader.readShort()
  val attributes_count: Short = reader.readShort()
  val attributes: Array[AttributeInfo] = (1 to attributes_count).map(_ => {
    AttributeInfo.load(reader, cf, method = this)
  }).toArray

  def name: String = cpv(name_index).value.toString

  def descriptor: String = cpv(descriptor_index).value.toString

  def codes: Array[Op] = code.code

  def paramsType: Array[String] = {
    val contentRe = """\((.*)\).*""".r
    val content = descriptor match {
      case contentRe(c) => c
    }
    val re = """((\[?[BCDFIJSZ])|(\[?L.+?;))""".r
    re.findAllMatchIn(content).map(_.group(0)).toArray
  }

  def returnType: String = {
    val re = """\(.*\)(.*)""".r
    descriptor match {
      case re(t) => t
    }
  }

  def code: CodeAttribute = attributes.find(_.isInstanceOf[CodeAttribute]).get.asInstanceOf[CodeAttribute]

  def signatures: Array[SignatureAttribute] = attributes.filter(_.isInstanceOf[SignatureAttribute]).map(_.asInstanceOf[SignatureAttribute])

  override def toString: String = {
    s"[Method] ${accessFlagsValue.mkString(",")} ${descriptor} ${name} ${signatures.map(_.toString).mkString(",")}\n" +
      s"${attributes.map(_.toString).mkString("\n")}"
  }

  override def accessMaskMap = Map(
    0x0001 -> "ACC_PUBLIC",
    0x0002 -> "ACC_PRIVATE",
    0x0004 -> "ACC_PROTECTED",
    0x0008 -> "ACC_STATIC",
    0x0010 -> "ACC_FINAL",
    0x0020 -> "ACC_SYNCHRONIZED",
    0x0040 -> "ACC_BRIDGE",
    0x0080 -> "ACC_VARARGS",
    0x0100 -> "ACC_NATIVE",
    0x0400 -> "ACC_ABSTRACT",
    0x0800 -> "ACC_STRICT",
    0x1000 -> "ACC_SYNTHETIC",
  )
}
