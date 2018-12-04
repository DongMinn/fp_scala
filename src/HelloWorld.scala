/**
  * Created by alvin on 2018. 9. 13..       
  */

import scala.math._

object HelloWorld extends App {
  //  println("Hello, World!")
  //
  //
  //  println(16.toHexString)
  //
  //  println("bar".take(2))

  //  def square(x: Double) = x * x
  //
  //  def area(radius: Double) = 3.14159 * square(radius)

  def fib(n: Int): Int = {
    def go(n: Int): Int = {
      if (n <= 1) 0
      else if (n == 2) 1
      else go(n - 1) + go(n - 2)
    }
    go(n)
  }

  println(fib(8))


 private  def fib2(n: Int): Int = {
    def go(n: Int): Int =
      if (n <= 0) 0
      else if (n == 1) 0
      else if (n == 2) 1
      else go(n - 1) + go(n - 2)

    go(n)
  }

  println(fib2(8))

}