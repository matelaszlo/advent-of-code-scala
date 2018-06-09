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
}
