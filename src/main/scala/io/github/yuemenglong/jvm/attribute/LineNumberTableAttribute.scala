package io.github.yuemenglong.jvm.attribute

import io.github.yuemenglong.jvm.common.{JvmItem, StreamReader}
import io.github.yuemenglong.jvm.struct.{AttributeInfo, ClassFile}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
class LineNumberTableAttribute(reader: StreamReader,
                               override val cf: ClassFile,
                               override val attribute_name_index: Short,
                               override val attribute_length: Int
                              ) extends AttributeInfo {
  val line_number_table_length: Short = reader.readShort()
  val line_number_table: Array[LineNumberTable] = (1 to line_number_table_length).map(_ => {
    new LineNumberTable(reader, cf)
  }).toArray

  override def toString = {
    s"${name} ${line_number_table_length}\n" +
      line_number_table.map(_.toString).mkString("\n")
  }
}

class LineNumberTable(reader: StreamReader,
                      override val cf: ClassFile
                     ) extends JvmItem {
  val start_pc: Short = reader.readShort()
  val line_number: Short = reader.readShort()

  override def toString = {
    s"${start_pc} ${line_number}"
  }
}
