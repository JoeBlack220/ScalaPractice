package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(b.apply() * b.apply() - 4 * a.apply() * c.apply())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var(
      if(delta.apply() < 0) Set()
      else if(delta.apply() == 0) Set(-b.apply() / (2 * a.apply()))
      else Set((-b.apply() + Math.sqrt(delta.apply()) / (2 * a.apply())), (-b.apply() - Math.sqrt(delta.apply()) / (2 * a.apply())))
    )
  }
}
