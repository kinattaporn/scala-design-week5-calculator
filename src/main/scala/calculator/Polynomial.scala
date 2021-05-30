package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b() - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val delta = computeDelta(a, b, c)
    Signal{
      var result = Set[Double]()
      if (delta() >= 0) {
        result += ((-b() + Math.sqrt(delta())) / (2 * a()))
        result += ((-b() - Math.sqrt(delta())) / (2 * a()))
      }
      else result
      result
    }
  }
}
