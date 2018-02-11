package io.github.yuemenglong.jvm

import java.nio.ByteBuffer

import scala.collection.mutable.ArrayBuffer

/**
  * Created by <yuemenglong@126.com> on 2018/2/11.
  */

/**
  * Created by <yuemenglong@126.com> on 2018/2/8.
  */
object CpInfo {
  def load(reader: StreamReader, count: Int, cf: ClassFile): Array[CpInfo] = {
    var ret = new ArrayBuffer[CpInfo]()
    ret += null
    var pos = 1
    while (pos < count) {
      val tag = reader.readByte()
      val info = tag match {
        case 1 => new ConstantUtf8Info
        case 3 => new ConstantIntegerInfo
        case 4 => new ConstantFloatInfo
        case 5 => new ConstantLongInfo
        case 6 => new ConstantDoubleInfo
        case 7 => new ConstantClassInfo
        case 8 => new ConstantStringInfo
        case 9 => new ConstantFieldrefInfo
        case 10 => new ConstantMethodrefInfo
        case 11 => new ConstantInterfaceMethodrefInfo
        case 12 => new ConstantNameAndTypeInfo
        case 15 => new ConstantMethodHandleInfo
        case 16 => new ConstantMethodTypeInfo
        case 18 => new ConstantInvokeDynamicInfo
      }
      info.read(reader)
      ret += info
      pos += 1
      if (Array(5, 6).indexOf(tag) >= 0) {
        ret += null
        pos += 1
      }
    }
    ret.toArray
  }

  def debug(arr: Array[CpInfo]): Unit = {
    val str = arr.zipWithIndex.map { case (info, idx) =>
      if (info == null) {
        "Null"
      } else {
        s"[${info.ty}] [${info.value(arr)}]"
      }
    }.mkString("\n")
    println(str)
  }
}

trait CpInfo {
  val tag: Byte

  def read(reader: StreamReader)

  def value(arr: Array[CpInfo]): Any

  def ty: String = {
    this match {
      case _: ConstantUtf8Info => "Utf8"
      case _: ConstantIntegerInfo => "Integer"
      case _: ConstantFloatInfo => "Float"
      case _: ConstantLongInfo => "Long"
      case _: ConstantDoubleInfo => "Double"
      case _: ConstantClassInfo => "Class"
      case _: ConstantStringInfo => "String"
      case _: ConstantFieldrefInfo => "Fieldref"
      case _: ConstantMethodrefInfo => "Methodref"
      case _: ConstantInterfaceMethodrefInfo => "InterfaceMethodref"
      case _: ConstantNameAndTypeInfo => "NameAndType"
      case _: ConstantMethodHandleInfo => "MethodHandle"
      case _: ConstantMethodTypeInfo => "MethodType"
      case _: ConstantInvokeDynamicInfo => "InvokeDynamic"
    }
  }
}

class ConstantUtf8Info extends CpInfo {
  override val tag = 1
  var length: Short = _
  var bytes: Array[Byte] = _

  override def read(reader: StreamReader): Unit = {
    length = reader.readShort()
    bytes = reader.readBytes(length)
  }

  override def value(arr: Array[CpInfo]): Any = new String(bytes)
}

class ConstantIntegerInfo extends CpInfo {
  override val tag = 3
  var bytes: Int = _

  override def read(reader: StreamReader): Unit = {
    bytes = reader.readInt()
  }

  override def value(arr: Array[CpInfo]) = bytes
}

class ConstantFloatInfo extends CpInfo {
  override val tag = 4
  var bytes: Float = _

  override def read(reader: StreamReader): Unit = {
    bytes = reader.readFloat()
  }

  override def value(arr: Array[CpInfo]) = bytes
}

class ConstantLongInfo extends CpInfo {
  override val tag = 5
  var high_bytes: Int = _
  var low_bytes: Int = _

  override def value(arr: Array[CpInfo]): Long = ByteBuffer.allocate(8).putInt(low_bytes).putInt(high_bytes).getLong()

  override def read(reader: StreamReader): Unit = {
    high_bytes = reader.readInt()
    low_bytes = reader.readInt()
  }
}

class ConstantDoubleInfo extends CpInfo {
  override val tag = 6
  var high_bytes: Int = _
  var low_bytes: Int = _

  override def value(arr: Array[CpInfo]): Double = ByteBuffer.allocate(8).putInt(low_bytes).putInt(high_bytes).getDouble()

  override def read(reader: StreamReader): Unit = {
    high_bytes = reader.readInt()
    low_bytes = reader.readInt()
  }
}

class ConstantClassInfo extends CpInfo {
  override val tag = 7
  var name_index: Short = _

  override def read(reader: StreamReader): Unit = {
    name_index = reader.readShort()
  }

  override def value(arr: Array[CpInfo]) = arr(name_index).value(arr)
}

class ConstantStringInfo extends CpInfo {
  override val tag = 8
  var string_index: Short = _

  override def read(reader: StreamReader): Unit = {
    string_index = reader.readShort()
  }

  override def value(arr: Array[CpInfo]) = arr(string_index).value(arr)
}

class ConstantFieldrefInfo extends CpInfo {
  override val tag = 9
  var class_index: Short = _
  var name_and_type_index: Short = _

  override def read(reader: StreamReader): Unit = {
    class_index = reader.readShort()
    name_and_type_index = reader.readShort()
  }

  override def value(arr: Array[CpInfo]) = arr(class_index).value(arr) + " | " + arr(name_and_type_index).value(arr)
}

class ConstantMethodrefInfo extends CpInfo {
  override val tag = 10
  var class_index: Short = _
  var name_and_type_index: Short = _

  override def read(reader: StreamReader): Unit = {
    class_index = reader.readShort()
    name_and_type_index = reader.readShort()
  }

  override def value(arr: Array[CpInfo]) = arr(class_index).value(arr) + " | " + arr(name_and_type_index).value(arr)
}

class ConstantInterfaceMethodrefInfo extends CpInfo {
  override val tag = 11
  var class_index: Short = _
  var name_and_type_index: Short = _

  override def read(reader: StreamReader): Unit = {
    class_index = reader.readShort()
    name_and_type_index = reader.readShort()
  }

  override def value(arr: Array[CpInfo]) = arr(class_index).value(arr) + " | " + arr(name_and_type_index).value(arr)
}

class ConstantNameAndTypeInfo extends CpInfo {
  override val tag = 12
  var name_index: Short = _
  var descriptor_index: Short = _

  override def read(reader: StreamReader): Unit = {
    name_index = reader.readShort()
    descriptor_index = reader.readShort()
  }

  override def value(arr: Array[CpInfo]) = arr(name_index).value(arr) + " | " + arr(descriptor_index).value(arr)
}

class ConstantMethodHandleInfo extends CpInfo {
  override val tag = 15
  var reference_kind: Byte = _
  var reference_index: Short = _

  override def read(reader: StreamReader): Unit = {
    reference_kind = reader.readByte()
    reference_index = reader.readShort()
  }

  override def value(arr: Array[CpInfo]) = reference_kind + " | " + arr(reference_index).value(arr)
}

class ConstantMethodTypeInfo extends CpInfo {
  override val tag = 16
  var descriptor_index: Short = _

  override def read(reader: StreamReader): Unit = {
    descriptor_index = reader.readShort()
  }

  override def value(arr: Array[CpInfo]) = arr(descriptor_index).value(arr)
}

class ConstantInvokeDynamicInfo extends CpInfo {
  override val tag = 18
  var bootstrap_method_attr_index: Short = _
  var name_and_type_index: Short = _

  override def read(reader: StreamReader): Unit = {
    bootstrap_method_attr_index = reader.readShort()
    name_and_type_index = reader.readShort()
  }

  override def value(arr: Array[CpInfo]) = arr(bootstrap_method_attr_index).value(arr) + " | " + arr(name_and_type_index).value(arr)
}
