sealed trait List[+A] //형식A에 대해 매개변수화된 List자료형식
case object Nil extends List[Nothing]

// 빈목록을 나타내는 List자료 생성자
case class Cons[+A](head: A, tail: List[A]) extends List[A]

//비지 않은 목록을 나타내는 또 다른 자료 생성자. tail은 또 다른 List[A]로, Nil일수도 있고 다른 Cons일 수도 있다.

/*
List동반(companion)객체. 목록의 생성과 조작을 위한 함수들을 담는다.
*/
object List {
  def sum(ints: List[Int]): Int = ints match {
    //패턴 부합을 이용해 목록의 정수들을 합하는 함수
    case Nil => 0 // 빈 목록의 합은 0
    case Cons(x, xs) => x + sum(xs) // x로 시작하는 목록의 합은 x더하기 목록 나머지 부분의 합
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = {
    foldRight(ns, 0)((x, y) => x + y)
  }

  def product2(ns: List[Double]) = {
    foldRight(ns, 1.0)(_ * _)
  }


  //3-2번
  //문제: List의 첫 요소를 제거하는 함수 만드시오.
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  //3-3번
  //문제: List의 첫 요소를 다른 값으로 대체하는 함수 만드시오.
  def setHead[A](l: List[A], y: A): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(y, xs)
  }

  //3-4번
  //문제: 처음 n개의 요소를 제거하는 함수 만드시오.
  def drop[A](l: List[A], n: Int): List[A] = {

    if (n > 0) {
      l match {
        case Nil => Nil
        case Cons(x, xs) => drop(xs, n - 1)
      }
    }
    else l
  }

  //3-5번
  //문제: 주어진 술어?(predicate)에 부합하는 List의 앞 요소를 제거하는 함수 만드시오.
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) dropWhile(xs, f)
      else Cons(x, dropWhile(xs, f))
  }

  //3-6
  //문제: 마지막 요소를 제외한 모든 요소로 이루어진List를 돌려주는 함수 만드시오.
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  //3-9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((x, y) => y + 1)
  }

  //3-10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  //3-11
  def sumLeft(ns: List[Int]): Int = {
    foldLeft(ns, 0)((x, y) => x + y)
  }

  //3-11
  def productLeft(ns: List[Double]): Double = {
    foldLeft(ns, 1.0)(_ * _)
  }

  //3-11
  def lengthLeft[A](ns: List[A]): Int = {
    foldLeft(ns, 0)((x, y) => x + 1)
  }

  //3-12
  def reverseList[A](ns: List[A]): List[A] = {
    foldLeft(ns, Nil: List[A])((x, y) => Cons(y, x))
  }


  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def apply[A](as: A*): List[A] = // 가변인수 함수 구문
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}


/*
* 3-1 검증 // 결과:  3
* 문제: 아래 결과는?
* */
val x = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}


/*
* 3-2 검증
 */
println("3-2 검증")
val x2 = List(1, 2, 3, 4, 5)
println(List.tail(x2))


/*
* 3-3 검증
 */
println("3-3 검증")
val x3 = List(1, 2, 3, 4, 5)
println(List.setHead(x3, 6))



/*
* 3-4 검증
 */
println("3-4 검증")
val x4 = List(1, 2, 3, 4, 5)
println(List.drop(x4, 2))


/*
* 3-5 검증
 */
println("3-4 검증")
val x5 = List(1, 2, 3, 4, 5)
def testFunction(n: Int): Boolean = {
  if (n == 1) true
  else if (n == 3) true
  else false
}

println(List.dropWhile(x5, testFunction))


/*
3-7
-> 바로 멈추지 않는다.
->재귀를 멈추는 조건이 Nil이기 때문이다.
foldRight로 sum을 추적(trace)한것 처럼, 반드시 끝까지 순회, 를 해야지만 결과를 알 수 있다.
마찬가지로 product도 0.0이 사이에 나온다 하여 즉지 결과를 아는것이 아닌, 끝까지 순회를 마친 후
결과를 알게 된다.
 */

/*
3-8
-> 그냥 List(1,2,3)이 나온다.
->
 */

/*
3-11검증
* */
println("3-11 검증")
val x11 = List(1, 2, 3, 4, 5)
println(List.sumLeft(x11))











//3-1
//val x = List(1,2,3,4,5)
//
//x match {
//  case x::2::4::_ => x
//  case Nil => 42
//  case x::y::3::4::_ => x+y
//  case x::y => x*x
//  case _ => 101
//}