package io.github.yuemenglong.jvm.common

import java.nio.ByteBuffer

/**
  * Created by <yuemenglong@126.com> on 2018/2/8.
  */
class StreamReader(seq: Seq[Byte]) {
  var s = seq

  private def readT[T](len: Int, fn: ByteBuffer => T): T = {
    val wrap = ByteBuffer.wrap(readBytes(len))
    fn(wrap)
  }

  def readBytes(len: Int): Array[Byte] = {
    val arr = s.slice(0, len).toArray
    s = s.drop(len)
    arr
  }

  def length: Long = s.length

  def isEmpty: Boolean = s.isEmpty

  def readByte(): Byte = readT(1, _.get())

  def readShort(): Short = readT(2, _.getShort())

  def readInt(): Int = readT(4, _.getInt())

  def readLong(): Long = readT(8, _.getLong())

  def readFloat(): Float = readT(4, _.getFloat())

  def readDouble(): Double = readT(8, _.getDouble())

  def readString(len: Int): String = new String(readBytes(len))
}
