/*
4장
* */

sealed trait Option[+A] {
  /*
  4-1 번 문제
  * */
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case None => None
      case Some(a) => f(a)
    }
  }

  //  def flatMap[B](f: A=> Option[B]) : Option[B]
  //  // 만일 Option이 None이 아니면 f(실패할 수 있음) 을 적용한다.
  //  def getOrElse[B >: A](default: =>B) :B
  //  //B >: A 는 B 형식 매개변수가 반드시 A 의 상위형식 이어야 함을 의미한다.
  //  def orElse[B >: A](ob: => Option[B]): Option[B]
  //  //ob는 필요한 경우에만 평가한다.
  //  def filter(f: A=> Boolean) : Option[A]
  //값이 f를 만족하지 않으면 Some 을 None으로 변환한다.
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

}

