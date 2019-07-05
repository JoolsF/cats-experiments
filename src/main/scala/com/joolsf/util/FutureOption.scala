package com.joolsf.util

import com.joolsf.ServiceError

import scala.concurrent.{ExecutionContext, Future}

object FutureOption {

  type FutureOption[A] = Future[Option[A]]

  /**
    * If you want to throw an exception if particular option returns None then
    * this might make more sense using a OptionT[Future,A].
    *
    * The net result is going from Future[Option[A]] => Future[A] and failing with a Failed future.
    *
    * Equivalent to FutureOptionT's logIfNone
    */
  def exceptionIfNone[A](f: Future[Option[A]])(error: ServiceError)(implicit ec: ExecutionContext): Future[A] =
    f.flatMap {
      case Some(v) =>
        Future.successful(v)
      case None =>
        println(s"Logging error: ${error.errorMessage}")
        Future.failed(throw new RuntimeException(error.errorMessage))
    }

}
