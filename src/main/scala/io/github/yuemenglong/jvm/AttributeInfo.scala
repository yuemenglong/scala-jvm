package io.github.yuemenglong.jvm

/**
  * Created by <yuemenglong@126.com> on 2018/2/11.
  */
class AttributeInfo(reader: StreamReader, val cf: ClassFile) {
  val attribute_name_index: Short = reader.readShort()
  val attribute_length: Int = reader.readInt()
  val info: Array[Byte] = reader.readBytes(attribute_length)
}
