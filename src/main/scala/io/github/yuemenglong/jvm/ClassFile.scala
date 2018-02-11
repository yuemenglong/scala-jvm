package io.github.yuemenglong.jvm

/**
  * Created by <yuemenglong@126.com> on 2018/2/11.
  */
class ClassFile(reader: StreamReader) {
  val magic: Int = reader.readInt()
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
    new AttributeInfo(reader, this)
  }).toArray
}
