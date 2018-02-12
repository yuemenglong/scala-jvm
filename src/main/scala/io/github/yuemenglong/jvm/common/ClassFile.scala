package io.github.yuemenglong.jvm.common

import io.github.yuemenglong.jvm.item.{AttributeInfo, CpInfo, FieldInfo, MethodInfo}


class ClassFile(reader: StreamReader) extends AccessFlagName {
  val magic: Int = reader.readInt()
  require(magic == 0xCAFEBABE)
  val minor_version: Short = reader.readShort()
  val major_version: Short = reader.readShort()
  val constant_pool_count: Short = reader.readShort()
  val constant_pool: Array[CpInfo] = CpInfo.load(reader, constant_pool_count, this)
  val access_flags: Short = reader.readShort()
  val this_class: Short = reader.readShort()
  val super_class: Short = reader.readShort()
  val interfaces_count: Short = reader.readShort()
  val interfaces: Array[Short] = (1 to interfaces_count).map(_ => {
    reader.readShort()
  }).toArray
  val fields_count: Short = reader.readShort()
  val fields: Array[FieldInfo] = (1 to fields_count).map(_ => {
    new FieldInfo(reader, this)
  }).toArray
  val methods_count: Short = reader.readShort()
  val methods: Array[MethodInfo] = (1 to methods_count).map(_ => {
    new MethodInfo(reader, this)
  }).toArray
  val attribute_count: Short = reader.readShort()
  val attributes: Array[AttributeInfo] = (1 to attribute_count).map(_ => {
    AttributeInfo.load(reader, this)
  }).toArray

  override def toString: String = {
    Array(
      accessFlagsValue.mkString(", "),
      thisClassValue,
      superClassValue,
      "[INTERFACE]",
      interfacesValue.mkString(", "),
      "[METHOD]",
      methods.map(_.toString).mkString("\n"),
      "[ATTRIBUTE]",
      attributes.map(_.toString).mkString("\n"),
    ).mkString("\n")
  }

  def thisClassValue: String = constant_pool(this_class).value.toString

  def superClassValue: String = constant_pool(super_class).value.toString

  def interfacesValue: Array[String] = interfaces.map(i => {
    constant_pool(i).value.toString
  })

  override def accessMaskMap = Map(
    0x0001 -> "ACC_PUBLIC",
    0x0010 -> "ACC_FINAL",
    0x0020 -> "ACC_SUPER",
    0x0200 -> "ACC_INTERFACE",
    0x0400 -> "ACC_ABSTRACT",
    0x1000 -> "ACC_SYNTHETIC",
    0x2000 -> "ACC_ANNOTATION",
    0x4000 -> "ACC_ENUM",
  )
}
