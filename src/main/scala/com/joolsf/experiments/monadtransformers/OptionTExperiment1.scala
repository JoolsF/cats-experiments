package com.joolsf.experiments.monadtransformers

import cats.data.OptionT
import cats.implicits._
import com.joolsf.util.FutureOption
import com.joolsf.{Logger, ResourceNotFoundError, ServiceError}

import scala.concurrent.{ExecutionContext, Future}

object OptionTExperiment1 extends Logger {

  import com.joolsf.TestService1._

  /**
    * To refactor - Non OptionT example
    * Need to check if first call returns None explictly.
    * Have to throw an exception.
    * Function is not total - signature is not accurate.
    */

  def example(id: Int)(implicit ec: ExecutionContext) = {
    for {
      maybeResource <- getResource(id)
      _ = if (maybeResource.isEmpty) logError(ResourceNotFoundError(id))
      resource = maybeResource.getOrElse(throw new RuntimeException(s"Resource $id does not exist"))
      resourceUpdated <- processResource(resource)
      result = processValue(resourceUpdated.value)
    } yield result
  }

  /**
    * Refactor 1
    *
    * Fetching an optional item and logging an error on failure i.e.
    * producing a side effect.
    *
    * A return type that now reflects the fact that this is a total function
    */


  def refactor1(id: Int)(implicit ec: ExecutionContext): Future[Option[Int]] = {
    for {
      resource <- OptionT(getResource(id)).orElse {
        logError(ResourceNotFoundError(id))
        OptionT.fromOption[Future](None)
      }
      resourceUpdated <- OptionT.liftF(processResource(resource))
      result <- OptionT.pure[Future](processValue(resourceUpdated.value))
    } yield result
  }.value


  /**
    * Refactor 2
    * Same as above but with FutureOption type and helper
    */
  def refactor2(id: Int)(implicit ec: ExecutionContext): Future[Option[Int]] = {
    import com.joolsf.util.FutureOptionT._
    for {
      resource <- logIfNone(getResource(1))(ResourceNotFoundError(id))
      resourceUpdated <- liftF(processResource(resource))
      result <- pure(processValue(resourceUpdated.value))
    } yield result
  }.value


  /**
    * Refactor 3
    * Transform return type to Either so that call site can deal with error.
    * Deal with Option internally with OptionT and improve function signature with a more descriptive Future[Either[...]]
    * return type.
    * Remove log as this could logged further up call stack.
    */

  def refactor3(id: Int)(implicit ec: ExecutionContext): Future[Either[ServiceError, Int]] = {
    import com.joolsf.util.FutureOptionT._
    toEither(ResourceNotFoundError(id)) {
      for {
        resource <- OptionT(getResource(1))
        resourceUpdated <- liftF(processResource(resource))
        result <- pure(processValue(resourceUpdated.value))
      } yield result
    }
  }


  /**
    * Refactor 4
    * If we literally just want to fail fast with an exception on the getResource method
    * we could write a helper to avoid the boiler plate and OptionT
    */
  def refactor4(id: Int)(implicit ec: ExecutionContext): Future[Int] =
    for {
      resource <- FutureOption.exceptionIfNone(getResource(id))(s"Resource $id does not exist")
      resourceUpdated <- processResource(resource)
      result = processValue(resourceUpdated.value)
    } yield result

}
