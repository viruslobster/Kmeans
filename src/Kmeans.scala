import scala.collection.mutable.ArraySeq
import scala.collection.mutable.ListBuffer
import scala.util.Random

class Kmeans(k: Int, d: Int) {
  var data = ListBuffer[Array[Double]]()
  var centriods = List[Array[Double]]()
  var clustered_data = List[ListBuffer[Array[Double]]]()

  def getK = k
  def addData(vector: Array[Double]) =
    if (vector.length == d) data += vector
    else throw new Exception("Input vector of wrong dimension!")

  def init() {
    centriods = ranSelect(data.toList, k)
    clustered_data = for (i <- List.range(0, k)) yield ListBuffer[Array[Double]]()
  }

  def assign() {
    clustered_data = for (i <- List.range(0, k)) yield ListBuffer[Array[Double]]()
    data foreach { (x: Array[Double]) =>
      val dists = for (i <- List.range(0, k)) yield dist(x, centriods(i))
      clustered_data(dists indexOf (dists min)) += x
    }
  }

  def update() = centriods = for (i <- List.range(0, k)) yield ave(clustered_data(i).toList)

  def ave(list: List[Array[Double]]): Array[Double] =
    list.reduceLeft((x: Array[Double], y: Array[Double]) =>
      for (i <- Array.range(0, x.length)) yield x(i) + y(i)).map(_ / list.length)

  def dist(p1: Array[Double], p2: Array[Double]) =
    math sqrt (for (i <- List.range(0, p1.length)) yield math.pow(p1(i) - p2(i), 2)).sum

  def ranSelect[T](list: List[T], i: Int): List[T] =
    i match {
      case _ if list == Nil => Nil
      case 0 => Nil
      case x =>
        val v = list(new Random().nextInt(list.size))
        v :: ranSelect(list diff List(v), i - 1)
    }
}