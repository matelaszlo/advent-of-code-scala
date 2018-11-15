package com.lmat.util

object Maths {

  /**
    * Simple primality test
    * Tests if a number is prime or not
    */
  def isPrime(n: Int): Boolean =
    if (n <= 1) false
    else (2 to math.sqrt(n).toInt).forall(n % _ != 0)

  /**
    * Tests if a number is composite or not
    */
  def isComposite(n: Int): Boolean = !isPrime(n)

  /**
    * All positive divisors
    * 60 => Set(1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60)
    */
  def divisors(n: Int): Set[Int] = {
    val half = (1 to math.sqrt(n).toInt).filter(n % _ == 0)
    (half ++ half.map(n / _)).toSet
  }
}
