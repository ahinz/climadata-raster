package climadata.raster2

import language.implicitConversions

class FloatArrayWrapper(val a: Array[Float]) extends ArrayAccessor[Float] {
  def set(i: Int, v: Float) = a.update(i, v)
  def get(i: Int): Float = a(i)
  def map(f: Float => Float) = {
    val l = a.length
    var i = 0
    val a2 = Array.ofDim[Float](l)
    val a1 = a
    while(i < l) {
      a2(i) = f(a1(i))
      i += 1

    }
    new FloatArrayWrapper(a2)
  }

  def getData() = a
}

class ByteArrayWrapper(val a: Array[Byte]) extends ArrayAccessor[Byte] {
  def set(i: Int, v: Byte) = a.update(i, v)
  def get(i: Int): Byte = a(i)
  def map(f: Byte => Byte) = {
    val l = a.length
    var i = 0
    val a2 = Array.ofDim[Byte](l)
    val a1 = a
    while(i < l) {
      a2(i) = f(a1(i))
      i += 1

    }
    new ByteArrayWrapper(a2)
  }

  def getData() = a
}

class DoubleArrayWrapper(val a: Array[Double]) extends ArrayAccessor[Double] {
  def set(i: Int, v: Double) = a.update(i, v)
  def get(i: Int): Double = a(i)
  def map(f: Double => Double) = {
    val l = a.length
    var i = 0
    val a2 = Array.ofDim[Double](l)
    val a1 = a
    while(i < l) {
      a2(i) = f(a1(i))
      i += 1

    }
    new DoubleArrayWrapper(a2)
  }

  def getData() = a
}


class ShortArrayWrapper(val a: Array[Short]) extends ArrayAccessor[Short] {
  def set(i: Int, v: Short) = a.update(i, v)
  def get(i: Int): Short = a(i)
  def map(f: Short => Short) = {
    val l = a.length
    var i = 0
    val a2 = Array.ofDim[Short](l)
    val a1 = a
    while(i < l) {
      a2(i) = f(a1(i))
      i += 1

    }
    new ShortArrayWrapper(a2)
  }

  def getData():Array[Short] = a
}

class IntArrayWrapper(val a: Array[Int]) extends ArrayAccessor[Int] {
  def set(i: Int, v: Int) = a.update(i, v)
  def get(i: Int): Int = a(i)
  def map(f: Int => Int) = {
    val l = a.length
    var i = 0
    val a2 = Array.ofDim[Int](l)
    val a1 = a
    while(i < l) {
      a2(i) = f(a1(i))
      i += 1

    }
    new IntArrayWrapper(a2)
  }
  def getData():Array[Int] = a
}

object ArrayAccessor {
  implicit def intArrayIsWrapable(f: Array[Int]):ArrayAccessor[Int] = new IntArrayWrapper(f)
  implicit def doubleArrayIsWrapable(f: Array[Double]):ArrayAccessor[Double] = new DoubleArrayWrapper(f)
  implicit def shortArrayIsWrapable(f: Array[Short]):ArrayAccessor[Short] = new ShortArrayWrapper(f)
  implicit def floatArrayIsWrapable(f: Array[Float]):ArrayAccessor[Float] = new FloatArrayWrapper(f)
  implicit def byteArrayIsWrapable(f: Array[Byte]):ArrayAccessor[Byte] = new ByteArrayWrapper(f)
}

trait ArrayAccessor[@specialized T] {
  def set(i: Int, v: T):Unit
  def get(i: Int): T
  def map(f: T => T): ArrayAccessor[T]

  def getData():Array[T]
}

