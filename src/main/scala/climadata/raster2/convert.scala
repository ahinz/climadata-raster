package climadata.raster2

trait RasterConverter[@specialized A, @specialized B] {
  def convert(a: Raster[A]):Raster[B]
}

object RasterConverter {
  implicit object Int2Double extends RasterConverter[Int,Double] {
    def convert(a: Raster[Int]):Raster[Double] = {
      val data1:Array[Int] = a.data.getData
      val l = data1.length
      val data2:Array[Double] = Array.ofDim[Double](l)
      var i = 0
      while(i < l) { data2(i) = data1(i).toDouble; i += 1 }
      Raster(data2, a.rows, a.cols)
    }
  }
}
