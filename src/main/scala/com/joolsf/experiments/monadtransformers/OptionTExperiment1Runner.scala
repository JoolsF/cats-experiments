package com.joolsf.experiments.monadtransformers

//TODO get rid of and write unit tests
object OptionTExperiment1Runner extends App {

  import OptionTExperiment1._

  import scala.concurrent.ExecutionContext.Implicits.global

  //  example(1) //throws an exception
  refactor1(1).value.map(println)

  refactor1(1).value.map(println)

}
