package com.joolsf.experiments.monadtransformers

import cats.data.EitherT
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object EitherTExperiment1 extends App {

  trait MyError

  case class FooError() extends MyError

  case class BarError() extends MyError

  case class Foo(i: Int)

  case class Bar(i: Int)


  def getFoo(succeed: Boolean = true): Future[Either[MyError, Foo]] =
    Future {
      if (succeed) {
        Right(Foo(10))
      } else {
        throw new IndexOutOfBoundsException
      }
    }

  def getBar(succeed: Boolean = true): Future[Either[MyError, Bar]] =
    if (succeed) {
      Future(Right(Bar(10)))
    } else {
      Future(Left(BarError()))
    }

  def run(): EitherT[Future, MyError, Int] = {
    for {
      foo <- EitherT(getFoo(false))
      bar <- EitherT(getBar())
    } yield foo.i + bar.i

  }

  val res = run.leftFlatMap {
    case error: FooError => EitherT[Future, MyError, Int](Future(Right(1)))
    case error: BarError => EitherT[Future, MyError, Int](Future(Right(2)))
  }

  res
    .value
    .recover { case e: IndexOutOfBoundsException => 3 }
    .map(println)

  Thread.sleep(2000)


}
