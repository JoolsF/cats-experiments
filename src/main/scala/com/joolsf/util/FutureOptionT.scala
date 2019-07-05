package com.joolsf.util

import cats.data.OptionT
import cats.implicits._
import com.joolsf.ServiceError
import com.joolsf.experiments.monadtransformers.OptionTExperiment1.logError

import scala.concurrent.{ExecutionContext, Future}

object FutureOptionT {

  type FutureOption[A] = OptionT[Future, A]

  def logIfNone[A](f: Future[Option[A]])(error: ServiceError)(implicit ec: ExecutionContext): FutureOption[A] =
    OptionT(f).orElse {
      logError(error)
      fromOption(Option.empty[A])
    }

  def liftF[A](f: Future[A])(implicit ec: ExecutionContext): FutureOption[A] = OptionT.liftF(f)

  def fromOption[A](v: Option[A])(implicit ec: ExecutionContext): FutureOption[A] = OptionT.fromOption[Future](v)

  def pure[A](v: A)(implicit ec: ExecutionContext): FutureOption[A] = OptionT.pure[Future](v)

  def toEither[A, B](leftCase: ServiceError)(f: OptionT[Future, A])(implicit executionContext: ExecutionContext): Future[Either[ServiceError, A]] =
    f.value.map {
      case Some(v) => Right(v)
      case None => Left(leftCase)
    }

  /**
    * Similar to FutureOption exceptionIfNone but going from OptionT[Future, A] => Future[A].
    * Useful if you're composing with OptionT[Future, A] but want to return Future[A] ultimately.
    */
  def exceptionIfNone[A](f: OptionT[Future, A])(errorMsg: String)(implicit ec: ExecutionContext): Future[A] =
    f.value.flatMap {
      case Some(v) =>
        Future.successful(v)
      case None =>
        println(s"Logging error: $errorMsg")
        Future.failed(throw new RuntimeException(errorMsg))
    }


}
