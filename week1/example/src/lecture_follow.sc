def sqrt(x: Double) = {

  def abs(x: Double) = if (x < 0) -x else x

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double) = {
    abs((x - guess * guess) / x) < 0.001
  }

  def improve(guess: Double) = (guess + x / guess) / 2

  sqrtIter(1.0)
}

sqrt(1.0e50)
sqrt(2)


def factorial(n: Int): Int = {
  def loop(acc: Int, n: Int):Int = {
    if (n == 0) acc else loop(acc * n, n - 1)
  }
  loop(1, n)
}

factorial(5)




