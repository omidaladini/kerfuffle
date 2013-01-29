import Task._
import Engine._

object Kerfuffle {

  case class Kerfuffle[A <: Task[_]](task: A) {
    def map[B <: Task[_]](f: A => B): Kerfuffle[B] = Kerfuffle(f(task))
    def flatMap[B <: Task[_]](f: A => Kerfuffle[B]): Kerfuffle[B] = f(task)
  }

  implicit def conv[A](a: Task[A]) : Kerfuffle[Task[A]] = Kerfuffle(a)
}
