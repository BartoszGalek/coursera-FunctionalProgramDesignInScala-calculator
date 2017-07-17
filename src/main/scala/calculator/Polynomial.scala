package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b()-4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = computeDelta(a,b,c)() match {
    case computed if computed > 0 => Signal{
      Set(
        (-b() + math.sqrt(computeDelta(a,b,c)())) / (2*a()),
        (-b() - math.sqrt(computeDelta(a,b,c)())) / (2*a())
      )
    }
    case _ => Signal{
      Set((-b()) / (2*a()))
    }
  }
}
