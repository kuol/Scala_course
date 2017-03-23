def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc:Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

sum((x:Int) => x, 1, 5)
sum((x:Int) => x*x, 2,5)
sum((x:Int) => x*2, 1, 5)

4 + 9 + 16 + 25

// define a curried function
def product(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1
  else f(a) * product(f)(a+1, b)
}

product((x:Int) => x)(1, 5)

def fact(n: Int) = product(x => x)(1, n)
fact(4)


def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:Int): Int = {
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
}

class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator cannot be zero.")

  // The 2nd constructor
  def this(x: Int) = this(x, 1)
  private def gcd(a: Int, b: Int):Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }
  private val g = gcd(x, y)
  def numer = x
  def denom = y

  def <(that: Rational): Boolean = numer * that.denom < denom * that.numer

  def neg = new Rational(-x, y)

  def add(that: Rational): Rational = {
    new Rational(that.numer * denom + that.denom * numer, that.denom * denom)
  }

  def sub(that: Rational): Rational = add(that.neg)

  override def toString = numer/g + "/" + denom/g
}

val x = new Rational(2,6)
x.numer
x.denom
val y = new Rational(5,7)
y.numer
y.denom

val z = new Rational(3,2)
println(x.add(y).toString)

println(x.sub(y).sub(z))
println(y.add(y))


(new Rational(1,2)) < (new Rational(3,4))

