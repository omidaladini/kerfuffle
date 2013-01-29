object KerfuffleTest extends App {

  import Kerfuffle._
  import Task._
  import Engine._

  def five(): Int = 5
  def multFive(a: Int): Double = a * 5 * 0.1
  def sum(a: Int, b: Double): String = (a + b).toString
  def six(): String = "6"
  def mult(a: Int, b: String): String = a.toString + "*" + b

  val kerf = for {
    a <- task(five)
    b <- task(multFive, a)
    c <- task(sum, a, b)
    d <- task(six)
    e <- task(mult, a, c)
  } yield e

  println(run(kerf.task))

}
