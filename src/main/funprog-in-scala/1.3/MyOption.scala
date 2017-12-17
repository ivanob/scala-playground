
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
      None
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
      None
    }
    def filter(f: A => Boolean): Option[A] = this match {
      case None => None
      case Some(x) => if(f(x)) Some(x) else None
    }
    def filter2(f: A => Boolean): Option[A] = {
      None
    }
  }
  case object None extends Option[Nothing]
  case class Some[+A](get: A) extends Option[A]
}
