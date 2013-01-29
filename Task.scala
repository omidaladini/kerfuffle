object Task {
  sealed trait Task[A]

  case class Task0[A](f: A) extends Task[A]
  case class Task1[A,B](f: Function1[A,B], d1:Task[A]) extends Task[B]
  case class Task2[A,B,C](f: Function2[A,B,C], d1:Task[A], d2: Task[B]) extends Task[C]

  def task[A](f: A) : Task[A] = Task0(f)
  def task[A,B](f: Function1[A,B], d1: Task[A]) : Task1[A,B] = Task1(f, d1)
  def task[A,B,C](f: Function2[A,B,C], d1: Task[A], d2: Task[B]) : Task2[A,B,C] = Task2(f, d1, d2)

}
