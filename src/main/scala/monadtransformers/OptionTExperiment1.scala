package monadtransformers

object OptionTExperiment1 {
  import cats.data.OptionT
  import cats.implicits._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import scala.concurrent.{Await, Future}
  import cats.implicits._

  /*
   * Example 1a
   * Fetching an optional item and logging an error on failure i.e.
   * producing a side effect.
   */
  def getOptionalInt(id: String): Future[Option[Int]] = Future {
    if (id == "1") Some(10) else None
  }

  def processInt(i: Int): Future[Int] = Future {
    i * 2
  }

  val res1: OptionT[Future, Int] = for {
    a <- OptionT(getOptionalInt("")).orElse {
      println("boo")
      OptionT(Future.successful(Option.empty[Int]))
    }
  } yield a

  Await.result(res1.value, 5 seconds)

  /*
   * Example 1b
   * Same as above but with helper
   */

  type FutureOption[A] = OptionT[Future, A]

  object FutureOption {
    def logIfEmpty[A](f: Future[Option[A]])(logMsg: String): FutureOption[A] = {
      OptionT(f).orElse {
        println(logMsg)
        OptionT(Future.successful(Option.empty[A]))
      }
    }

    //  def pure[A](value: Future[A]) = OptionT.pure(value)

    //  def lift[A](fa: Future[A]) = OptionT.liftF[Future, A](fa)
    def toFutureOption[A](f: Future[A]): OptionT[Future, A] = OptionT(f.map(x => Option(x)))

  }

  import FutureOption._

  val res2: OptionT[Future, Int] = for {
    a <- logIfEmpty(getOptionalInt("1"))("oh no")
    b <- toFutureOption(processInt(a))
  } yield b

  res2.map(println)

  Thread.sleep(5000)
}
