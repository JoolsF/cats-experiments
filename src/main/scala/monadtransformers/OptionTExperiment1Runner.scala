package monadtransformers

object OptionTExperiment1Runner extends App {
  import OptionTExperiment1._

//  example(1) //throws an exception
  refactor1(1).value.map(println)

  refactor1(1).value.map(println)

}
