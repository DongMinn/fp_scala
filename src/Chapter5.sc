sealed trait Stream[+A]{
  /*
  5-1 Stream을 List로 변환
  */
  def toList: List[A] = this match{
    case Empty =>  Nil
    case Cons(h,t) => h()::t().toList
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

/*비지 않은 스트림은 하나의 머리와 하나의 꼬리로 구성된다. 둘다 엄격하지 않은 값인데, 기술적인 한계 때문에
  이들은 이름으로 전달되는 인수가 아니라 반드시 명시적으로 강제해야 하는 thunk이다.
*/
object Stream {
  //비지 않은 스트림의 생성을 위한 똑똑한 생성자.
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    // 평가반복을 피하기 위해 head와tail을 게으른 값으로 캐싱한다.
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  //특정 형식의 빈 스트림을 생성하기 위한 똑똑한 생성자.
  def empty[A]: Stream[A] = Empty

  //여러 요소로 이루어진 Stream의 생성을 위한 편의용 가변인수 메서드
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}