
object MyTree {
  sealed trait Tree[+A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  object Tree{
      def size[A](t: Tree[A]):Int = t match {
        case Branch(l,r) => size(l) + size(r)
        case Leaf(_) => 1
      }
  }
}
