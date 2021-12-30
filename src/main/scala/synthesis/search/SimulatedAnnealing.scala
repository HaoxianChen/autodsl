package synthesis.search

import scala.util.Random

object SimulatedAnnealing {
  val rnd = new Random()
  def uniformSample[T](set: Set[T]): T = set.toList(rnd.nextInt(set.size))

  /** T is the temperature function.
   * Always 1 when e2 > e1
   * When e2 < e1, T the higher, the acceptanceProbability is higher
   *  */
  def acceptanceProbability(e1: Double, e2: Double, T: Double)  :Double = {
    require(T>0)
    if (e2 > e1) {
      1
    }
    else {
      math.pow(math.E,-(e1-e2)/T)
    }
  }


  def sample[T](candidates: Set[T], getScore: T=>Double, baseScore :Double = 0): T = {
    require(candidates.nonEmpty)
    def temperature(r: Double): Double = {
      require(r>0 && r<=1)
      1*r
    }
    val K = 20 // Max number of random walks to sample next candidate
    var i: Int = 0
    var next: Option[T] = None
    while (next.isEmpty && i<K) {
      val n = uniformSample(candidates)
      val T = temperature(1.0  - i.toDouble/K)
      if (acceptanceProbability(baseScore,getScore(n),T) > rnd.nextInt(1)) {
        next = Some(n)
      }
      i += 1
    }
    assert(next.isDefined, s"Failed to sample next candidate after $K random walks.")
    next.get
  }

}
