package io.codeswarm

import scala.language.postfixOps

object Quicksort {

  def quicksortFunctional(a: Array[Int]): Array[Int] =
    if (a.length <= 1) a
    else quicksortFunctional(a filter (a(0) >)) ++ (a filter (a(0) ==)) ++ quicksortFunctional(a filter (a(0) <))

  def quicksortImperative(a: Array[Int]) {
    def swap(i: Int, j: Int) {
      val t = a(i)
      a(i) = a(j)
      a(j) = t
    }
    def sort(l: Int, r: Int) {
      val pivot = a((l + r) / 2)
      var i = l
      var j = r
      while (i <= j) {
        while (a(i) < pivot) i += 1
        while (a(j) > pivot) j -= 1
        if (i <= j) {
          swap(i, j)
          i += 1
          j -= 1
        }
      }
      if (l < j) sort(l, j)
      if (j < r) sort(i, r)
    }
    sort(0, a.length - 1)
  }

  // Example of use case
  def main(args: Array[String]) {
    val N = 10000000
    var a = scala.util.Random.shuffle((0 until N).asInstanceOf[Seq[Int]]).toArray

    val start = System.currentTimeMillis
    a = quicksortFunctional(a)
    println(System.currentTimeMillis - start)

//    val startImperative = System.currentTimeMillis
//    quicksortImperative(a)
//    println(System.currentTimeMillis - startImperative

    (0 until N).foreach(i => assert(i == a(i)))
  }
}
