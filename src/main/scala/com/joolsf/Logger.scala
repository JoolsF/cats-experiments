package com.joolsf

trait Logger {
  // To simulate loggings
  def logError(serviceError: ServiceError) = println(serviceError.errorMessage)

}
