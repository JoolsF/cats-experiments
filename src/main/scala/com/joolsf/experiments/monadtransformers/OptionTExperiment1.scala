package com.joolsf.experiments.monadtransformers

import cats.data.OptionT
import cats.implicits._
import com.joolsf.util.FutureOption
import com.joolsf.{ApiRequestError, GenericError, Logger, ResourceNotFoundError, ServiceError}

import scala.concurrent.{ExecutionContext, Future}

object OptionTExperiment1 extends Logger {

  import com.joolsf.TestService1._

  /**
    * To refactor - Non OptionT example
    * Need to check if first call returns None explicitly.
    * Have to throw an exception.
    * Function is not total - signature is not accurate.
    */

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
      number <- OptionT(apiRequest).orElse {
        logError(ApiRequestError("foo"))
        OptionT.fromOption[Future](None)
      }

      resourceUpdated <- OptionT.liftF(processResource(resource))
      result <- OptionT.pure[Future](processValues(resourceUpdated.value, number))
    } yield result
  }.value


  /**
    * Refactor 2
    * Same as above but with FutureOptionT type and helper
    */
  def refactor2(id: Int)(implicit ec: ExecutionContext): Future[Option[Int]] = {
    import com.joolsf.util.FutureOptionT._
    for {
      resource <- logIfNone(getResource(1))(ResourceNotFoundError(id))
      number <- logIfNone(apiRequest())(ApiRequestError("foo"))
      resourceUpdated <- liftF(processResource(resource))
      result <- pure(processValues(resourceUpdated.value, number))
    } yield result
  }.value


  /**
    * Refactor 3
    * Transform return type to Either so that call site can deal with error.
    * Deal with Option internally with OptionT and improve function signature with a more descriptive Future[Either[...]]
    * return type.
    *
    * Issue here is that at the level of the ServiceError we don't know what error to generate as the None can
    * can from several places.  We are logging however
    */
  def refactor3(id: Int)(implicit ec: ExecutionContext): Future[Either[ServiceError, Int]] = {
    import com.joolsf.util.FutureOptionT._
    toEither(GenericError("error")) {
      for {
        resource <- OptionT(getResource(1))
        number <- logIfNone(apiRequest())(ApiRequestError("foo"))
        resourceUpdated <- liftF(processResource(resource))
        result <- pure(processValues(resourceUpdated.value, number))
      } yield result
    }
  }


  /**
    * Refactor 5
    * If we want to keep return type of the method the same then
    * this is one approach to getting rid of the Option.
    * 
    * Similar issue to above in terms of the 'GenericError'
    */
  def refactor4(id: Int)(implicit ec: ExecutionContext): Future[Int] = {
    import com.joolsf.util.FutureOptionT._
    toFuture(GenericError("error")) {
      for {
        resource <- logIfNone(getResource(1))(ResourceNotFoundError(id))
        number <- logIfNone(apiRequest())(ApiRequestError("foo"))
        resourceUpdated <- liftF(processResource(resource))
        result <- pure(processValues(resourceUpdated.value, number))
      } yield result
    }
  }

  /**
    * Refactor 6
    * Same as above but with implicit class syntax
    */
  import com.joolsf.util.FutureOptionTSyntax._
  def refactor6(id: Int)(implicit ec: ExecutionContext): Future[Int] = {
    import com.joolsf.util.FutureOptionT._
      for {
        resource <- logIfNone(getResource(1))(ResourceNotFoundError(id))
        number <- logIfNone(apiRequest())(ApiRequestError("foo"))
        resourceUpdated <- liftF(processResource(resource))
        result <- pure(processValues(resourceUpdated.value, number))
      } yield result
  }.toFuture(GenericError("error"))

  /**
    * Refactor 7
    * If we literally just want to fail fast with an exception on the getResource method
    * we could write a helper to avoid the boiler plate and OptionT
    */
  def refactor7(id: Int)(implicit ec: ExecutionContext): Future[Int] =
    for {
      resource <- FutureOption.toFuture(getResource(id))(ResourceNotFoundError(id))
      number <- FutureOption.toFuture(apiRequest())(ApiRequestError("foo"))
      resourceUpdated <- processResource(resource)
      result = processValues(resourceUpdated.value, number)
    } yield result

}
