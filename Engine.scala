import Task._

object Engine {
  def run[A](t: Task[A]) : A = {
    //TODO: Make a topological order and then run it in parallel!! yay!
    t match {
      case Task0(f) => f
      case Task1(f, d1) => f(run(d1))
      case Task2(f, d1, d2) => f(run(d1), run(d2))
    }
  }
}
