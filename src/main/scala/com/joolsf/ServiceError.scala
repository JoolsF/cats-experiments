package com.joolsf

trait ServiceError {
  val httpStatus: Int
  val errorMessage: String

}

case class ResourceNotFoundError(id: Int) extends ServiceError {
  val httpStatus = 404
  val errorMessage = s"Resource $id does not exist"
}

case class ApiRequestError(apiName: String) extends ServiceError {
  val httpStatus = 500
  val errorMessage = s"Request to $apiName api failed"
}

case class GenericError(msg: String) extends ServiceError {
  val httpStatus = 500
  val errorMessage = msg
}
