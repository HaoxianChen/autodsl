package synthesis

case class Evaluator(edb: Examples) {
  private var cache: Map[Program, Examples] = Map()
  def _eval(program: Program): Examples = {
    // check if results are cached
    if (cache.contains(program)) cache(program)
    else {
      // make temporary directory

      // dump problem to file

      // run

      // load results from file
      val idb: Examples = ???

      // update cache
      cache = cache.updated(program, idb)
      idb
    }
  }

  def eval(program: Program): Set[Tuple] = _eval(program).toTuples()
}
