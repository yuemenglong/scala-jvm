package io.github.yuemenglong.jvm

/**
  * Created by <yuemenglong@126.com> on 2018/2/11.
  */
class MethodInfo(reader: StreamReader, cf: ClassFile) {
  val access_flags: Short = reader.readShort()
  val name_index: Short = reader.readShort()
  val descriptor_index: Short = reader.readShort()
  val attributes_count: Short = reader.readShort()
  val attributes: Array[AttributeInfo] = (1 to attributes_count).map(_ => {
    new AttributeInfo(reader,cf)
  }).toArray
}

object MethodInfo {

}