object rationals {
  val x = new Rational(1, 2)
  val y = new Rational(2, 3)
  val z = new Rational(3, 5)
  x + y
  x.sub(y).sub(z)
  x < y
  x max y

}

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")
  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)

  val numer = x / g
  val denom = y / g

  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this < that) this else that

  def +(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)
  override def toString = numer + "/" + denom

  def neg: Rational = new Rational(-numer, denom)

  def sub(that: Rational) = this + that.neg

}