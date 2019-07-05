package com.joolsf.util

import cats.data.{EitherT, OptionT}
import cats.implicits._
import com.joolsf.{Logger, ServiceError}

import scala.concurrent.{ExecutionContext, Future}

object FutureEitherT extends Logger {

  type FutureEitherT[A] = EitherT[Future, ServiceError, A]

  def apply[A](left: ServiceError)(f: Future[Option[A]])(implicit ec: ExecutionContext): FutureEitherT[A] =
    OptionT(f).toRight(left)

}
