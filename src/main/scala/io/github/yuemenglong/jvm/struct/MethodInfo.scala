package io.github.yuemenglong.jvm.struct

import io.github.yuemenglong.json.lang.JsonIgnore
import io.github.yuemenglong.jvm.attribute.method.{CodeAttribute, SignatureAttribute}
import io.github.yuemenglong.jvm.common.{AccessFlagName, StreamReader}

/**
  * Created by <yuemenglong@126.com> on 2018/2/11.
  */
@JsonIgnore(Array("reader", "cf"))
class MethodInfo(reader: StreamReader, cf: ClassFile) extends AccessFlagName {
  val access_flags: Short = reader.readShort()
  val name_index: Short = reader.readShort()
  val descriptor_index: Short = reader.readShort()
  val attributes_count: Short = reader.readShort()
  val attributes: Array[AttributeInfo] = (1 to attributes_count).map(_ => {
    AttributeInfo.load(reader, cf, method = this)
  }).toArray

  def nameValue: String = cf.constant_pool(name_index).value.toString

  def descriptorValue: String = cf.constant_pool(descriptor_index).value.toString

  def code: CodeAttribute = attributes.find(_.isInstanceOf[CodeAttribute]).get.asInstanceOf[CodeAttribute]

  def signatures: Array[SignatureAttribute] = attributes.filter(_.isInstanceOf[SignatureAttribute]).map(_.asInstanceOf[SignatureAttribute])

  override def toString: String = {
    s"${accessFlagsValue.mkString(",")} ${descriptorValue} ${nameValue}\n" +
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
