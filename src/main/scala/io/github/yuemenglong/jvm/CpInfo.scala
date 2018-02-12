package io.github.yuemenglong.jvm

import java.nio.ByteBuffer

import io.github.yuemenglong.json.lang.JsonIgnore

import scala.collection.mutable.ArrayBuffer

/**
  * Created by <yuemenglong@126.com> on 2018/2/11.
  */
object CpInfo {
  def load(reader: StreamReader, count: Int, cf: ClassFile): Array[CpInfo] = {
    val ret = new ArrayBuffer[CpInfo]()
    ret += null
    var pos = 1
    while (pos < count) {
      val tag = reader.readByte()
      val info = tag match {
        case 1 => new ConstantUtf8Info(reader, cf)
        case 3 => new ConstantIntegerInfo(reader, cf)
        case 4 => new ConstantFloatInfo(reader, cf)
        case 5 => new ConstantLongInfo(reader, cf)
        case 6 => new ConstantDoubleInfo(reader, cf)
        case 7 => new ConstantClassInfo(reader, cf)
        case 8 => new ConstantStringInfo(reader, cf)
        case 9 => new ConstantFieldrefInfo(reader, cf)
        case 10 => new ConstantMethodrefInfo(reader, cf)
        case 11 => new ConstantInterfaceMethodrefInfo(reader, cf)
        case 12 => new ConstantNameAndTypeInfo(reader, cf)
        case 15 => new ConstantMethodHandleInfo(reader, cf)
        case 16 => new ConstantMethodTypeInfo(reader, cf)
        case 18 => new ConstantInvokeDynamicInfo(reader, cf)
      }
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
    val str = arr.map { (info) =>
      if (info == null) {
        "Null"
      } else {
        s"[${info.ty}] [${info.value}]"
      }
    }.mkString("\n")
    println(str)
  }
}

@JsonIgnore(Array("reader", "cf"))
trait CpInfo {
  val tag: Byte

  def value: Any

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

class ConstantUtf8Info(reader: StreamReader, cf: ClassFile) extends CpInfo {
  override val tag = 1
  val length: Short = reader.readShort()
  val bytes: Array[Byte] = reader.readBytes(length)

  override def value: Any = new String(bytes)
}

class ConstantIntegerInfo(reader: StreamReader, cf: ClassFile) extends CpInfo {
  override val tag = 3
  val bytes: Int = reader.readInt()

  override def value = bytes
}

class ConstantFloatInfo(reader: StreamReader, cf: ClassFile) extends CpInfo {
  override val tag = 4
  val bytes: Float = reader.readFloat()

  override def value = bytes
}

class ConstantLongInfo(reader: StreamReader, cf: ClassFile) extends CpInfo {
  override val tag = 5
  val high_bytes: Int = reader.readInt()
  val low_bytes: Int = reader.readInt()

  override def value: Long = ByteBuffer.allocate(8).putInt(low_bytes).putInt(high_bytes).getLong()
}

class ConstantDoubleInfo(reader: StreamReader, cf: ClassFile) extends CpInfo {
  override val tag = 6
  val high_bytes: Int = reader.readInt()
  val low_bytes: Int = reader.readInt()

  override def value: Double = ByteBuffer.allocate(8).putInt(low_bytes).putInt(high_bytes).getDouble()
}

class ConstantClassInfo(reader: StreamReader, cf: ClassFile) extends CpInfo {
  override val tag = 7
  val name_index: Short = reader.readShort()

  override def value = cf.constant_pool(name_index).value
}

class ConstantStringInfo(reader: StreamReader, cf: ClassFile) extends CpInfo {
  override val tag = 8
  val string_index: Short = reader.readShort()

  override def value = cf.constant_pool(string_index).value
}

class ConstantFieldrefInfo(reader: StreamReader, cf: ClassFile) extends CpInfo {
  override val tag = 9
  val class_index: Short = reader.readShort()
  val name_and_type_index: Short = reader.readShort()

  override def value = cf.constant_pool(class_index).value + " | " + cf.constant_pool(name_and_type_index).value
}

class ConstantMethodrefInfo(reader: StreamReader, cf: ClassFile) extends CpInfo {
  override val tag = 10
  val class_index: Short = reader.readShort()
  val name_and_type_index: Short = reader.readShort()

  override def value = cf.constant_pool(class_index).value + " | " + cf.constant_pool(name_and_type_index).value
}

class ConstantInterfaceMethodrefInfo(reader: StreamReader, cf: ClassFile) extends CpInfo {
  override val tag = 11
  val class_index: Short = reader.readShort()
  val name_and_type_index: Short = reader.readShort()

  override def value = cf.constant_pool(class_index).value + " | " + cf.constant_pool(name_and_type_index).value
}

class ConstantNameAndTypeInfo(reader: StreamReader, cf: ClassFile) extends CpInfo {
  override val tag = 12
  val name_index: Short = reader.readShort()
  val descriptor_index: Short = reader.readShort()

  override def value = cf.constant_pool(name_index).value + " | " + cf.constant_pool(descriptor_index).value
}

class ConstantMethodHandleInfo(reader: StreamReader, cf: ClassFile) extends CpInfo {
  override val tag = 15
  val reference_kind: Byte = reader.readByte()
  val reference_index: Short = reader.readShort()

  override def value = reference_kind + " | " + cf.constant_pool(reference_index).value
}

class ConstantMethodTypeInfo(reader: StreamReader, cf: ClassFile) extends CpInfo {
  override val tag = 16
  val descriptor_index: Short = reader.readShort()

  override def value = cf.constant_pool(descriptor_index).value
}

class ConstantInvokeDynamicInfo(reader: StreamReader, cf: ClassFile) extends CpInfo {
  override val tag = 18
  val bootstrap_method_attr_index: Short = reader.readShort()
  val name_and_type_index: Short = reader.readShort()

  override def value = cf.constant_pool(bootstrap_method_attr_index).value + " | " + cf.constant_pool(name_and_type_index).value
}
