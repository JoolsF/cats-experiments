package com.joolsf.experiments.monadtransformers

import cats.data.EitherT
import cats.implicits._
import com.joolsf.util.FutureEitherT
import com.joolsf.{ApiRequestError, Logger, ResourceNotFoundError, ServiceError}

import scala.concurrent.{ExecutionContext, Future}

object EitherTExperiment1 extends Logger {

  import com.joolsf.TestService1._

  def example(id: Int)(implicit ec: ExecutionContext): Future[Int] = {
    for {
      maybeResource1 <- getResource(id)
      _ = if (maybeResource1.isEmpty) println(s"Resource $id does not exist")
      resource = maybeResource1.getOrElse(throw new RuntimeException(s"Resource $id does not exist"))
      maybeNumber <- apiRequest()
      _ = if (maybeNumber.isEmpty) println(s"Request to api failed")
      number = maybeNumber.getOrElse(throw new RuntimeException(s"Request to third party failed"))
      resourceUpdated <- processResource(resource)
      result = processValues(resourceUpdated.value, number)
    } yield result
  }

  /**
    * Refactor 1
    * Taking the same methods as before involving Future[Option[A]] we could convert the types to Either
    *
    * Option Nones are tranformed to specific errors.  These can be logged at the end
    */

  def refactor1(id: Int)(implicit ec: ExecutionContext): Future[Either[ServiceError, Int]] = {
    for {
      resource <- FutureEitherT(ResourceNotFoundError(id))(getResource(id))
//      number <- FutureEitherT(ApiRequestError("foo"))(apiRequest()) //TODO fix
      resourceUpdated <- EitherT.liftF(processResource(resource))
      result = processValues(resourceUpdated.value, 1) //TODO fix
    } yield result
  }.leftMap { error => logError(error); error } //TODO could be improved
    .value


}
