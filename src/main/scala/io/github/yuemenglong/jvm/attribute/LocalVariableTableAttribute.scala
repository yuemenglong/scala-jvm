package io.github.yuemenglong.jvm.attribute

import io.github.yuemenglong.jvm.common.{ClassFile, StreamReader}
import io.github.yuemenglong.jvm.item.AttributeInfo

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
class LocalVariableTableAttribute(reader: StreamReader,
                                  override val cf: ClassFile,
                                  override val attribute_name_index: Short,
                                  override val attribute_length: Int
                                 ) extends AttributeInfo {

}

class LocalVariableTable{
  val start_pc
}
