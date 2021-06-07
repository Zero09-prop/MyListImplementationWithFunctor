package part2

import cats.Functor
import cats.implicits._

trait MyListImpl {
  sealed trait MyList[+T] {
    def foreach[U](f: T => U): Unit
    def map[T2](f: T => T2): MyList[T2]
    def toList: List[T]
  }
  object MyList {
    def apply[T](elems: T*): MyList[T] = {
      elems.toList match {
        case Nil => MyNil
        case _   => ~>(elems.head, apply(elems.tail: _*))
      }
    }
  }

  case class ~>[T](elem: T, next: MyList[T]) extends MyList[T] {
    override def foreach[U](f: T => U): Unit = {
      f(elem)
      next.foreach(f)
    }
    override def map[T2](f: T => T2): MyList[T2] = ~>[T2](f(elem), next.map(f))

    override def toList: List[T] = elem +: next.toList
  }
  case object MyNil extends MyList[Nothing] {
    override def foreach[U](f: Nothing => U): Unit = {}
    override def map[T2](f: Nothing => T2): MyList[T2] = MyNil
    override def toList: List[Nothing] = Nil
  }
}

trait Mappers {
  trait Mapper[L[_]] {
    def map[T, T2](list: L[T])(f: T => T2): L[T2]
  }
  object Mapper {
    def apply[L[_]: Mapper](implicit list: Mapper[L]) = {
      list
    }
  }
}

trait MappersImpl extends Mappers with MyListImpl {
  implicit val mapper1: Mapper[MyList] = new Mapper[MyList] {
    def map[T, T2](list: MyList[T])(f: T => T2): MyList[T2] = {
      list.map(f)
    }
  }
  implicit val mapper2: Mapper[List] = new Mapper[List] {
    def map[T, T2](list: List[T])(f: T => T2): List[T2] = {
      list.map(f)
    }
  }
  implicit val functor1: Functor[MyList] = new Functor[MyList] {
    override def map[A, B](fa: MyList[A])(f: A => B): MyList[B] = fa.map(f)
  }
}

object MyListDemo2 extends MappersImpl with MyListImpl {

  def multiplyList[L[_]: Functor](list: L[Int]) = {
    Functor[L].map(list)(_ * 2)
  }
  def doSomething[L[_]: Functor](l: L[Int]) = Functor[L].map(l)(_ * 5)

  def multiplyListDouble[L[_]](list: L[Int])(implicit functor: Functor[L]) = {
    functor.map(list)(_ * 3.7)
  }
  def divisionList[L[_]: Functor](list: L[Int]) = {
    implicitly[Functor[L]].map(list)(_ / 10.11)
  }

  def main(args: Array[String]): Unit = {
    MyList(1, 2, 3)
      .map(_ * 15)
      .foreach(x => {
        println(s"x = ${x}")
      })
    MyList(1, 2, 3) match {
      case head ~> tail => println(s"head = ${head}, tail = $tail")
      case MyNil        => println(s"MyNil")
    }
    println(s"multiplyList(MyList(1, 2, 3))(mapper1) = ${multiplyList(MyList(1, 2, 3))}")
    println(s"multiplyList(List(1, 2, 3))(mapper2) = ${multiplyList(List(1, 2, 3))}")
    println(s"multiplyListDouble(MyList(1, 2, 3))(mapper1) = ${multiplyListDouble(MyList(1, 2, 3))}")
    println(s"multiplyListDouble(List(1, 2, 3))(mapper2) = ${multiplyListDouble(List(1, 2, 3))}")
    println(s"divisionList(MyList(1, 2, 3))(mapper1) = ${divisionList(MyList(1, 2, 3))}")
    println(s"divisionList(List(1, 2, 3))(mapper2) = ${divisionList(List(1, 2, 3))}")
  }
}
