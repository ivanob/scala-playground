
object MyOption {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(x) => Some(f(x))
    }
    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(x) => f(x)
    }
    def flatMap2[B](f: A => Option[B]): Option[B] = {
      map(f).getOrElse(None)
    }
    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(x) => x
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case Some(x) => Some(x)
    }
    def orElse2[B >: A](ob: => Option[B]): Option[B] = {
      getOrElse(ob)
    }
    def filter(f: A => Boolean): Option[A] = this match {
      case None => None
      case Some(x) => if(f(x)) Some(x) else None
    }
    def filter2(f: A => Boolean): Option[A] = {
      None
    }

    def variance(xs: Seq[Double]): Option[Double] = {
      val mean = xs.fold(0)((x,acc) => x+acc)/xs.length
      val variance = xs.flatMap(x => math.pow(x-mean, 2))
    }
  }
  case object None extends Option[Nothing]
  case class Some[+A](get: A) extends Option[A]
}
