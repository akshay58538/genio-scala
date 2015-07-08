package com.paypal.genio

/**
 * Created by akgoel on 03/07/15.
 */
object Main extends App {
  var r = new Reader()
  r.readSpec("/swagger.yaml") match {
    case (SpecTypeGDD, parsedSpec) => {
      val gddService = new SpecGDD(parsedSpec)
      val serviceName = gddService.name
      println(serviceName)
    }
    case (SpecTypeSwagger, parsedSpec) => {
      val swaggerService = new SpecSwagger(parsedSpec)
      val serviceName = swaggerService.name
      println(serviceName)
    }
    case (_, _) => {
      println("Invalid Input")
    }
  }
}
