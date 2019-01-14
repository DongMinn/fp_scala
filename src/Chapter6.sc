

trait RNG {
  def nextInt: (Int, RNG)

  def doubleInt: (Int, Int)
}

object RNG {

  /*6-1*/
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    if (n < 0)
      (-(n + 1), nextRNG)
    else
      (n, nextRNG)
  }

  /*6-2*/
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    (n.toDouble / (Int.MaxValue.toDouble + 1), nextRNG)
  }

}

type Rand[+A] = RNG => (A, RNG)

def unit[A](a:A):Rand[A]= {
  rng=>(a,rng)
}



print(unit[Int](3))

def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
  rng => {
    val (a, rng2) = s(


      rng)
    (f(a), rng2)
  }
}
/*6-5*/
def doubleViaMap: Rand[Double] = {
  map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))
}
/*6-6*/
def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
  rng => {
    val (raa, rng1) = ra(rng)
    val (rba, rng2) = rb(rng1)
    (f(raa, rba), rng2)
  }
}


abstract case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    //&는 비트 단위 논리곱(AND) 이다. 현재 종잣값을 이용해서 새 종잣값을 만든다.
    val nextRNG = SimpleRNG(newSeed)
    // 다음 상태(새 종잣값으로 생성한 RNG 인스턴스)
    val n = (newSeed >>> 16).toInt
    //>>>는 빈자리를 0으로 채우는 이진 오른쪽 자리이동이다. 값n은 새 의사난수 정수이다.
    (n, nextRNG)
    //반환값은 의사난수 정수와 다음 발생기 상태를 담은 튜플이다.
  }
}

