import scala.annotation.tailrec

/*
* 2-1  0,1,1,2,3,5,8,13,......
* */

def fib(n: Int): Int = {
  @tailrec
  def go(n: Int, prev: Int, next: Int): Int = {
    if (n == 1) prev
    else go(n - 1, next, prev + next)
  }

  go(n, 0, 1)
}
/*검증*/
println("1번문제")
println(fib(5))
fib(1)
fib(8)

/*
* 2-2 Array[A]가 주어진 비교 함수에 의거해서 정렬되어 있는지 점검하는
* isSorted함수를 구현하라.
* */

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

  @tailrec
  def loop(n: Int): Boolean = {
    if (n >= as.length - 1) true
    else if (!ordered(as(n), as(n + 1))) false
    else loop(n + 1)
  }

  loop(0)
}

/*검증*/
def ordered(n: Int, m: Int): Boolean = {
  if (n > m) false
  else true
}
val as = Array(1, 3, 2)
println("2번문제")
println(isSorted(as, ordered))


/*
* 2-3 def curry[A,B,C](f:(A,B) => C) : A=>(B=>C)
* */

println("3번문제")
def curry[A, B, C](f: (A, B) => C): A => B => C = {
  a => b => f(a, b)
}

/*
* 2-4 def uncurry[A,B,C](f:A=>B=>C) : (A,B)=>C
* */

println("4번문제")
def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a, b) => (f(a)) (b)
  //    (a, b) => f(a) (b)
}

/*
* 2-5 def compose[A,B,C](f:B=>C , g: A=>B) : A=>C
* */
println("5번문제")
def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}