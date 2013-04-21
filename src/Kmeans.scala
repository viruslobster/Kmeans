import scala.collection.mutable.ArraySeq
import scala.collection.mutable.ListBuffer

class Kmeans(k: Int, d: Int) {
  var data = ListBuffer[Array[Double]]()
  var centriods = List[Array[Double]]()
  var prev_centriods = List[Array[Double]]()
  var clustered_data = List[ListBuffer[Array[Double]]]()

  def getK = k

  def addData(vector: Array[Double]) =
    if (vector.length == d) data += vector
    else throw new Exception("Input vector of wrong dimension!")

  def init() {
    centriods = Math.ranSelect(data.toList, k)
    clustered_data = for (i <- List.range(0, k)) yield ListBuffer[Array[Double]]()
  }

  def assign() {
    clustered_data = for (i <- List.range(0, k)) yield ListBuffer[Array[Double]]()
    data foreach { (x: Array[Double]) =>
      val dists = for (i <- List.range(0, k)) yield Math.dist(x, centriods(i))
      clustered_data(dists indexOf (dists min)) += x
    }
  }
  
  def converged(): Boolean = {
    if (prev_centriods == Nil) return false
    for (i <- 0 to centriods.length - 1) {
      if (!Math.arrayeq(centriods(i), prev_centriods(i))) return false
    }
    return true
  }

  def update() = {
    prev_centriods = centriods;
    centriods = for (i <- List.range(0, k)) yield Math.ave(clustered_data(i).toList)
  }

}