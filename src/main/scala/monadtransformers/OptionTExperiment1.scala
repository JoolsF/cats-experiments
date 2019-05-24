package monadtransformers

import cats.data.OptionT
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object OptionTExperiment1 {

  // Setup
  case class Resource(id: Int, value: Int)

  def logError(msg: String) = println(s"Error: $msg")

  trait ServiceError

  case class ResourceNotFoundError(msg: String) extends ServiceError

  // Methods

  def getResource(id: Int): Future[Option[Resource]] = Future {
    if (id == 1) None else Some(Resource(1, 20))
  }

  def processResource(r: Resource): Future[Resource] = Future {
    r.copy(value = r.value * 100)
  }

  def processValue(i: Int): Int = i * 3

  /**
    * To refactor - Non OptionT example
    * Need to check if first call returns None explictly.
    * Have to throw an exception.
    * Function is not total - signature is not accurate.
    */

  def example(id: Int) = {
    for {
      maybeResource <- getResource(id)
      _ = if (maybeResource.isEmpty) logError("Resource $id does not exist")
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


  def refactor1(id: Int) = {
    for {
      resource <- OptionT(getResource(id)).orElse {
        logError(s"Resource $id does not exist")
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

  type FutureOption[A] = OptionT[Future, A]

  object FutureOption {

    def logIfNone[A](f: Future[Option[A]])(message: String): FutureOption[A] =
      OptionT(f).orElse {
        logError(message)
        pure(Option.empty[A])
      }

    def liftF[A](f: Future[A]): FutureOption[A] = OptionT.liftF(f)

    def pure[A](v: Option[A]): FutureOption[A] = OptionT.fromOption[Future](v)

    def pure[A](v: A): FutureOption[A] = OptionT.pure[Future](v)

    def toEither[A](error: ServiceError)(f: FutureOption[A]): Future[Either[ServiceError, A]] =
      f.value.map {
        case Some(v) => Right(v)
        case None => Left(error)
      }
  }

  import FutureOption._

  def refactor2(id: Int): Future[Option[Int]] = {
    for {
      resource <- logIfNone(getResource(1))(s"Resource $id does not exist")
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

  def refactor3(id: Int): Future[Either[ServiceError, Int]] =
    toEither(ResourceNotFoundError(s"Resource $id does not exist")) {
      for {
        resource <- OptionT(getResource(1))
        resourceUpdated <- liftF(processResource(resource))
        result <- pure(processValue(resourceUpdated.value))
      } yield result
    }

}
