package com.joolsf

trait ServiceError {
  val httpStatus: Int
  val errorMessage: String

}

case class ResourceNotFoundError(id: Int) extends ServiceError {
  val httpStatus = 404
  val errorMessage = s"Resource $id does not exist"
}
