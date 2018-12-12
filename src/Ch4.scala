/**
  * Created by alvin on 09/12/2018.       
  */
object Ch4 extends App {

  sealed trait Option[+A] {
    /*
    4-1 번 문제
    * */
    def map[B](f: A => B): Option[B] = { //만일 Option이 None이 아니면 f를 적용한다.
      this match {
        case None => None
        case Some(a) => Some(f(a))
      }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = { // 만일 Option이 None이 아니면 f(실패할 수 있음) 을 적용한다.
      this match {
        case None => None
        case Some(a) => f(a)
      }
    }

    def getOrElse[B >: A](default: => B): B = { //B >: A 는 B 형식 매개변수가 반드시 A 의 상위형식 이어야 함을 의미한다.
      //Option의  Some 안의 결과를 돌려준다. 단 Option 이 None이면 기본값을 돌려준다.
      //default : => B   해당 인수의 형식이 B이지만 그 인수가 함수에서 실제로 쓰일 때까지 평가하지 않음. 비 엄격성 (다음 챕터)
      this match {
        case None => default
        case Some(a) => a
      }
    }

    //첫 Option 이 정의되저 있으면 그것을 돌려주고, 그렇지 않으면 두번째 Option을 돌려준다.
    def orElse[B >: A](ob: => Option[B]): Option[B] = { //ob는 필요한 경우에만 평가한다.
      this match {
        case None => ob
        case Some(a) => this
      }
    }

    def filter(f: A => Boolean): Option[A] = { //값이 f를 만족하지 않으면 Some 을 None으로 변환한다.
      this match {
        case Some(a) if f(a) => this
        case _ => None
      }
    }

    /*
    4-2문제
    * */
    def variance(xs: Seq[Double]): Option[Double] = {
      def average(seq: Seq[Double]): Option[Double] = {
        if (seq.isEmpty) None
        else Some(seq.sum / seq.length)
      }

      // - m = Seq의 평균, x = Seq의 각 요소
      // - variance = Seq의 각 요소 x에 대한 math.pow(x - m, 2) 값들의 평균.
      // 1. Seq의 평균 값 m을 구한다.
      // 2. Seq의 요소 x에 대한 match.pow(x - m, 2) 값들의 Seq를 구한다.
      // 3. 2번에서 구한 Seq의 평균 값을 구한다.

      // note: flatMap을 사용하면 average 함수의 리턴 값을 직접 매핑해서
      //       간결하게 표현할 수 있다.
      average(xs).flatMap(m => average(xs.map(x => math.pow(x - m, 2))))
    }


  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  object Option {


    def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

    def Try[A](a: => A): Option[A] =
      try Some(a)
      catch {
        case e: Exception => None
      }


    /*
    4-3문제
    * */
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a.flatMap(aa => b.map(bb => f(aa, bb)))
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] = {

    }

  }

}
