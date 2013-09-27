class Rational(n: Int, d: Int) {
  require(d != 0)
  val numer: Int = n
  val denom: Int = d

  override def toString = numer + "/" + denom

  def this(n: Int) = this(n, 1)

  def + (that: Rational): Rational =
    new Rational(
      numer * that.denom + denom * that.numer,
      denom * that.denom
    )
  def + (i: Int): Rational =
    new Rational(numer + denom * i, denom)

  def - (that: Rational): Rational =
    new Rational(
      numer * that.denom - denom * that.numer,
      denom * that.denom
    )

  def - (i: Int): Rational =
    new Rational(numer - i * denom, denom)

  def * (that: Rational): Rational =
    new Rational(numer * that.numer, denom * that.denom)

  def * (i: Int): Rational =
    new Rational(numer * i, denom)

  def / (that: Rational): Rational =
    new Rational(numer * that.denom, denom * that.numer)

  def / (i: Int): Rational =
    new Rational(numer, denom * i)

  def lessThan(that: Rational) =
    numer * that.denom < denom * that.numer

  def max(that: Rational) =
    if (this lessThan that) that else this
}

val oneHalf = new Rational(1, 2)
val twoThirds = new Rational(2, 3)

implicit def intToRational(x: Int) = new Rational(x)

println(oneHalf * 2)
println(2 * oneHalf)

