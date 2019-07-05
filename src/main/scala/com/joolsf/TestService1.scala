package com.joolsf

import scala.concurrent.{ExecutionContext, Future}

object TestService1 {
  // Methods
  def getResource(id: Int)(implicit ec: ExecutionContext): Future[Option[Resource]] = Future {
    if (id == 1) None else Some(Resource(1, 20))
  }

  def processResource(r: Resource)(implicit ec: ExecutionContext): Future[Resource] = Future {
    r.copy(value = r.value * 100)
  }

  def apiRequest()(implicit ec: ExecutionContext): Future[Option[Int]] = Future.successful(Some(1))

  def processValues(i: Int, j: Int)(implicit ec: ExecutionContext): Int = i * j
}
