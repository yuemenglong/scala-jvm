package io.github.yuemenglong.jvm.attribute.method

import io.github.yuemenglong.jvm.common.StreamReader
import io.github.yuemenglong.jvm.struct.{AttributeInfo, ClassFile, MethodAttributeInfo, MethodInfo}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
class SignatureAttribute(reader: StreamReader,
                         override val cf: ClassFile,
                         override val method: MethodInfo,
                         override val attribute_name_index: Short,
                         override val attribute_length: Int
                        ) extends MethodAttributeInfo {
  val signature_index: Short = reader.readShort()

  override def toString = {
    s"${name} ${cf.constant_pool(signature_index)}"
  }
}
