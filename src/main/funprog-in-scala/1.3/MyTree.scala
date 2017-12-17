
object MyTree {
  sealed trait Tree[+A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  object Tree{
    //Exercise 3.25
    def size[A](t: Tree[A]):Int = t match {
      case Branch(l,r) => size(l) + size(r)
      case Leaf(_) => 1
    }

    //Exercise 3.26
    def maximum(t: Tree[Int]): Int = t match {
      case Branch(l,r) => maximum(l) max maximum(r)
      case Leaf(x) => x
    }
    //Exercise 3.27
    def depth[A](t: Tree[A]):Int = t match {
      case Branch(l,r) => {
        val leftDepth = depth(l)
        val rightDepth = depth(r)
        if(leftDepth>rightDepth) leftDepth + 1 else rightDepth + 1
      }
      case Leaf(x) => 1
    }

    //Exercise 3.28
    def map[A,B](t: Tree[A])(f: A=>B): Tree[B] = t match {
      case Branch(l,r) => Branch(map(l)(f),map(r)(f))
      case Leaf(x) => Leaf(f(x))
    }

    //Exercise 3.29
    def fold[A,B](t: Tree[A], acc: B)(f: (A,B)=>B)(g: (B,B)=>B):B = t match {
      case Branch(l,r) => g(fold(l,acc)(f)(g), fold(r,acc)(f)(g))
      case Leaf(x) => f(x,acc)
    }
  }
}
