object Kerfuffle {

  sealed trait Task[A]

  case class Task0[A](f: A) extends Task[A]
  case class Task1[A](f: Function1[A,A], d1: Task[A]) extends Task[A]
  case class Task2[A](f: Function2[A,A,A], d1: Task[A], d2: Task[A]) extends Task[A]

  def task[A](f: A) : Task[A] = Task0(f)
  def task[A](f: Function1[A,A], d1: Task[A]) : Task1[A] = Task1(f, d1)
  def task[A](f: Function2[A,A,A], d1: Task[A], d2: Task[A]) : Task2[A] = Task2(f, d1, d2)

  case class Kerfuffle[A <: Task[_]](task: A) {
    def map[B <: Task[_]](f: A => B): Kerfuffle[B] = Kerfuffle(f(task))
    def flatMap[B <: Task[_]](f: A => Kerfuffle[B]): Kerfuffle[B] = f(task)
  }

  implicit def conv[A](a: Task[A]) : Kerfuffle[Task[A]] = Kerfuffle(a)

  def run[A](t: Task[A]) : A = {
    //TODO: Make a topological order and then run it in parallel!! yay!
    t match {
      case Task0(f) => f
      case Task1(f, d1) => f(run(d1))
      case Task2(f, d1, d2) => f(run(d1), run(d2))
    }
  }

}

object KerfuffleTest extends App {

  import Kerfuffle._

  def five(): Int = 5
  def multFive(a: Int): Int = a * 5
  def sum(a: Int, b: Int): Int = a + b
  def six(): Int = 6
  def mult(a: Int, b: Int): Int = a * b

  val kerf = for {
    a <- task(five)
    b <- task(multFive, a)
    c <- task(sum, a, b)
    d <- task(six)
    e <- task(mult, a, c)
  } yield e

  println(run(kerf.task))

}
