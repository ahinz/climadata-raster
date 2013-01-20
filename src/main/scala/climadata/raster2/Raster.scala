package climadata.raster2

import scala.reflect._
import language.implicitConversions

import scala.reflect.macros.Context
import scala.language.experimental.macros

object Raster {
  def apply[@specialized T](data:ArrayAccessor[T], cols:Int, rows:Int):Raster[T] =
    new Raster(data, cols, rows)
}

class Raster[@specialized T](val data: ArrayAccessor[T], val rows: Int, val cols: Int) {
  def apply(r: Int, c: Int):T = data.get(r*rows + c)

  def map(f: T => T) =
    Raster(data.map(f), rows, cols)

  def convert[C](implicit r: RasterConverter[T,C]) = r.convert(this)
}

object Operation {
  // This is gross... but w/e
  sealed class AInt
  implicit object AInt extends AInt
  sealed class ADouble
  implicit object ADouble extends ADouble

  def add(r1: Raster[Int], r2: Raster[Int])(implicit dummy: AInt) = {
    val d1 = r1.data.getData
    val d2 = r2.data.getData
    val l = d1.length
    val d3 = Array.ofDim[Int](l)
    var i = 0
    while(i < l) {
      d3(i) = d2(i) + d1(i)
      i += 1
    }
    new Raster(new IntArrayWrapper(d2), r1.rows, r1.cols)
  }

  def add(r1: Raster[Double], r2: Raster[Double])(implicit dummy: ADouble) = {
    val d1 = r1.data.getData
    val d2 = r2.data.getData
    val l = d1.length
    val d3 = Array.ofDim[Double](l)
    var i = 0
    while(i < l) {
      d3(i) = d2(i) + d1(i)
      i += 1
    }
    new Raster(new DoubleArrayWrapper(d2), r1.rows, r1.cols)
  }
}

