import scala.util.Random

object Math {
  def dist(p1: Array[Double], p2: Array[Double]) =
    math sqrt (for (i <- List.range(0, p1.length)) yield math.pow(p1(i) - p2(i), 2)).sum

  def ranSelect[T](list: List[T], i: Int): List[T] = {
    lazy val new_list = Random shuffle list
    new_list take i
  }

  def ave(list: List[Array[Double]]): Array[Double] =
    list.reduceLeft((x: Array[Double], y: Array[Double]) =>
      for (i <- Array.range(0, x.length)) yield x(i) + y(i)).map(_ / list.length)

  def arrayeq[T](a: Array[T], b: Array[T]): Boolean = {
    for (i <- 0 to a.length - 1) {
      if (a(i) != b(i)) return false
    }
    return true
  }
}