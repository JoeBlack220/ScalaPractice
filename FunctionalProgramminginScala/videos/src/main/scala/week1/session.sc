object test {
  def abs(x: Double) = if (x < 0) -x else x

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      abs(guess + x / guess) / 2

    sqrtIter(x)
  }

  def gcd(a: Int, b: Int) : Int =
    if (b == 0) a else gcd(b, a % b)  // tail recursive

  def factorial(n: Int): Int ={
    def loop(acc: Int, n: Int) :Int = {
      if(n == 0) acc
      else loop(acc * n, n - 1)
    }
    loop(1, n)
  }
  factorial(10)
  sqrt(1e60)
  gcd(14, 21)
}
