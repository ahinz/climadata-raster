package climadata.raster

import language.implicitConversions

trait ByteImplicits {
  implicit def byteRasterOps(lhs:Raster[Byte]) = new {
    def +(rhs:Raster[Short]):Raster[Short] = lhs.toShort + rhs
    def +(rhs:Raster[Int])(implicit i1:DI):Raster[Int] = lhs.toInt + rhs
    def +(rhs:Raster[Float])(implicit i1:DI, i2:DI):Raster[Float] = lhs.toFloat + rhs
    def +(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs.toDouble + rhs

    def +(rhs:Byte):Raster[Byte] = lhs + ByteConstantRaster(rhs, lhs.rows, lhs.cols)
    def +(rhs:Short):Raster[Short] = lhs.toShort + ShortConstantRaster(rhs, lhs.rows, lhs.cols)
    def +(rhs:Int):Raster[Int] = lhs.toInt + IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def +(rhs:Float):Raster[Float] = lhs.toFloat + FloatConstantRaster(rhs, lhs.rows, lhs.cols)
    def +(rhs:Double):Raster[Double] = lhs.toDouble + DoubleConstantRaster(rhs, lhs.rows, lhs.cols)

    def -(rhs:Raster[Short]):Raster[Short] = lhs.toShort - rhs
    def -(rhs:Raster[Int])(implicit i1:DI):Raster[Int] = lhs.toInt - rhs
    def -(rhs:Raster[Float])(implicit i1:DI, i2:DI):Raster[Float] = lhs.toFloat - rhs
    def -(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs.toDouble - rhs

    def -(rhs:Byte):Raster[Byte] = lhs - ByteConstantRaster(rhs, lhs.rows, lhs.cols)
    def -(rhs:Short):Raster[Short] = lhs.toShort - ShortConstantRaster(rhs, lhs.rows, lhs.cols)
    def -(rhs:Int):Raster[Int] = lhs.toInt - IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def -(rhs:Float):Raster[Float] = lhs.toFloat - FloatConstantRaster(rhs, lhs.rows, lhs.cols)
    def -(rhs:Double):Raster[Double] = lhs.toDouble - DoubleConstantRaster(rhs, lhs.rows, lhs.cols)

    def *(rhs:Raster[Short]):Raster[Short] = lhs.toShort * rhs
    def *(rhs:Raster[Int])(implicit i1:DI):Raster[Int] = lhs.toInt * rhs
    def *(rhs:Raster[Float])(implicit i1:DI, i2:DI):Raster[Float] = lhs.toFloat * rhs
    def *(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs.toDouble * rhs

    def *(rhs:Byte):Raster[Byte] = lhs * ByteConstantRaster(rhs, lhs.rows, lhs.cols)
    def *(rhs:Short):Raster[Short] = lhs.toShort * ShortConstantRaster(rhs, lhs.rows, lhs.cols)
    def *(rhs:Int):Raster[Int] = lhs.toInt * IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def *(rhs:Float):Raster[Float] = lhs.toFloat * FloatConstantRaster(rhs, lhs.rows, lhs.cols)
    def *(rhs:Double):Raster[Double] = lhs.toDouble * DoubleConstantRaster(rhs, lhs.rows, lhs.cols)

    def /(rhs:Raster[Short]):Raster[Short] = lhs.toShort / rhs
    def /(rhs:Raster[Int])(implicit i1:DI):Raster[Int] = lhs.toInt / rhs
    def /(rhs:Raster[Float])(implicit i1:DI, i2:DI):Raster[Float] = lhs.toFloat / rhs
    def /(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs.toDouble / rhs

    def /(rhs:Byte):Raster[Byte] = lhs / ByteConstantRaster(rhs, lhs.rows, lhs.cols)
    def /(rhs:Short):Raster[Short] = lhs.toShort / ShortConstantRaster(rhs, lhs.rows, lhs.cols)
    def /(rhs:Int):Raster[Int] = lhs.toInt / IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def /(rhs:Float):Raster[Float] = lhs.toFloat / FloatConstantRaster(rhs, lhs.rows, lhs.cols)
    def /(rhs:Double):Raster[Double] = lhs.toDouble / DoubleConstantRaster(rhs, lhs.rows, lhs.cols)

    def %(rhs:Raster[Short]):Raster[Short] = lhs.toShort % rhs
    def %(rhs:Raster[Int])(implicit i1:DI):Raster[Int] = lhs.toInt % rhs

    def %(rhs:Byte):Raster[Byte] = lhs % ByteConstantRaster(rhs, lhs.rows, lhs.cols)
    def %(rhs:Short):Raster[Short] = lhs.toShort % ShortConstantRaster(rhs, lhs.rows, lhs.cols)
    def %(rhs:Int):Raster[Int] = lhs.toInt % IntConstantRaster(rhs, lhs.rows, lhs.cols)

    def |(rhs:Raster[Short]):Raster[Short] = lhs.toShort | rhs
    def |(rhs:Raster[Int])(implicit i1:DI):Raster[Int] = lhs.toInt | rhs

    def |(rhs:Byte):Raster[Byte] = lhs | ByteConstantRaster(rhs, lhs.rows, lhs.cols)
    def |(rhs:Short):Raster[Short] = lhs.toShort | ShortConstantRaster(rhs, lhs.rows, lhs.cols)
    def |(rhs:Int):Raster[Int] = lhs.toInt | IntConstantRaster(rhs, lhs.rows, lhs.cols)

    def &(rhs:Raster[Short]):Raster[Short] = lhs.toShort & rhs
    def &(rhs:Raster[Int])(implicit i1:DI):Raster[Int] = lhs.toInt & rhs

    def &(rhs:Byte):Raster[Byte] = lhs & ByteConstantRaster(rhs, lhs.rows, lhs.cols)
    def &(rhs:Short):Raster[Short] = lhs.toShort & ShortConstantRaster(rhs, lhs.rows, lhs.cols)
    def &(rhs:Int):Raster[Int] = lhs.toInt & IntConstantRaster(rhs, lhs.rows, lhs.cols)
  }

  implicit def byte2rasterOps(lhs:Byte) = new {
    def +(rhs:Raster[Byte]):Raster[Byte] = new ByteConstantRaster(lhs, rhs.rows, rhs.cols) + rhs
    def +(rhs:Raster[Short])(implicit i1:DI):Raster[Short] = new ShortConstantRaster(lhs.toShort, rhs.rows, rhs.cols) + rhs
    def +(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Int] = new IntConstantRaster(lhs.toInt, rhs.rows, rhs.cols) + rhs
    def +(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Float] = new FloatConstantRaster(lhs.toFloat, rhs.rows, rhs.cols) + rhs
    def +(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs.toDouble, rhs.rows, rhs.cols) + rhs

    def -(rhs:Raster[Byte]):Raster[Byte] = new ByteConstantRaster(lhs, rhs.rows, rhs.cols) - rhs
    def -(rhs:Raster[Short])(implicit i1:DI):Raster[Short] = new ShortConstantRaster(lhs.toShort, rhs.rows, rhs.cols) - rhs
    def -(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Int] = new IntConstantRaster(lhs.toInt, rhs.rows, rhs.cols) - rhs
    def -(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Float] = new FloatConstantRaster(lhs.toFloat, rhs.rows, rhs.cols) - rhs
    def -(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs.toDouble, rhs.rows, rhs.cols) - rhs

    def *(rhs:Raster[Byte]):Raster[Byte] = new ByteConstantRaster(lhs, rhs.rows, rhs.cols) * rhs
    def *(rhs:Raster[Short])(implicit i1:DI):Raster[Short] = new ShortConstantRaster(lhs.toShort, rhs.rows, rhs.cols) * rhs
    def *(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Int] = new IntConstantRaster(lhs.toInt, rhs.rows, rhs.cols) * rhs
    def *(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Float] = new FloatConstantRaster(lhs.toFloat, rhs.rows, rhs.cols) * rhs
    def *(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs.toDouble, rhs.rows, rhs.cols) * rhs

    def /(rhs:Raster[Byte]):Raster[Byte] = new ByteConstantRaster(lhs, rhs.rows, rhs.cols) / rhs
    def /(rhs:Raster[Short])(implicit i1:DI):Raster[Short] = new ShortConstantRaster(lhs.toShort, rhs.rows, rhs.cols) / rhs
    def /(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Int] = new IntConstantRaster(lhs.toInt, rhs.rows, rhs.cols) / rhs
    def /(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Float] = new FloatConstantRaster(lhs.toFloat, rhs.rows, rhs.cols) / rhs
    def /(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs.toDouble, rhs.rows, rhs.cols) / rhs

    def %(rhs:Raster[Byte]):Raster[Byte] = new ByteConstantRaster(lhs, rhs.rows, rhs.cols) % rhs
    def %(rhs:Raster[Short])(implicit i1:DI):Raster[Short] = new ShortConstantRaster(lhs.toShort, rhs.rows, rhs.cols) % rhs
    def %(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Int] = new IntConstantRaster(lhs.toInt, rhs.rows, rhs.cols) % rhs

    def |(rhs:Raster[Byte]):Raster[Byte] = new ByteConstantRaster(lhs, rhs.rows, rhs.cols) | rhs
    def |(rhs:Raster[Short])(implicit i1:DI):Raster[Short] = new ShortConstantRaster(lhs.toShort, rhs.rows, rhs.cols) | rhs
    def |(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Int] = new IntConstantRaster(lhs.toInt, rhs.rows, rhs.cols) | rhs

    def &(rhs:Raster[Byte]):Raster[Byte] = new ByteConstantRaster(lhs, rhs.rows, rhs.cols) & rhs
    def &(rhs:Raster[Short])(implicit i1:DI):Raster[Short] = new ShortConstantRaster(lhs.toShort, rhs.rows, rhs.cols) & rhs
    def &(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Int] = new IntConstantRaster(lhs.toInt, rhs.rows, rhs.cols) & rhs
  }
}
