package io.github.yuemenglong.jvm

import io.github.yuemenglong.json.lang.JsonIgnore

/**
  * Created by <yuemenglong@126.com> on 2018/2/11.
  */
object AttributeInfo {
  def load(reader: StreamReader, cf: ClassFile): AttributeInfo = {
    val attribute_name_index = reader.readShort()
    val attribute_length = reader.readInt()
    val name = s"${cf.constant_pool(attribute_name_index).value}"
    name match {
      case "Code" => new CodeAttribute(reader, cf, attribute_name_index, attribute_length)
      case _ => new OtherAttribute(reader, cf, attribute_name_index, attribute_length)
    }
  }
}

@JsonIgnore(Array("reader", "cf"))
trait AttributeInfo {
  val cf: ClassFile
  val attribute_name_index: Short
  val attribute_length: Int

  def name: String = s"${cf.constant_pool(attribute_name_index).value}"

  override def toString = name
}

class OtherAttribute(reader: StreamReader,
                     override val cf: ClassFile,
                     override val attribute_name_index: Short,
                     override val attribute_length: Int,
                    ) extends AttributeInfo {
  val bytes: Array[Byte] = reader.readBytes(attribute_length)
}

class CodeAttribute(reader: StreamReader,
                    override val cf: ClassFile,
                    override val attribute_name_index: Short,
                    override val attribute_length: Int
                   ) extends AttributeInfo {
  val max_stack: Short = reader.readShort()
  val max_locals: Short = reader.readShort()
  val code_length: Int = reader.readInt()
  val code: Array[Byte] = reader.readBytes(code_length)
  val exception_table_length: Short = reader.readShort()
  val exception_table: Array[Any] = (1 to exception_table_length).map(_ => {
    new ExceptionTable(reader, cf)
  }).toArray
  val attributes_count: Short = reader.readShort()
  val attributes: Array[AttributeInfo] = (1 to attributes_count).map(_ => {
    AttributeInfo.load(reader, cf)
  }).toArray
}

class ExceptionTable(reader: StreamReader, cf: ClassFile) {
  val start_pc: Short = reader.readShort()
  val end_pc: Short = reader.readShort()
  val handler_pc: Short = reader.readShort()
  val catch_type: Short = reader.readShort()
}
