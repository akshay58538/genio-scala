package com.paypal.genio

/**
 * Created by akgoel on 03/07/15.
 */
object Main extends App {
  var r = new Reader()
  r.readSpec("/swagger.yaml") match {
    case (SpecTypeGDD, parsedSpec) => {
      val gddService = new ServiceGDD(parsedSpec)
      val serviceName = gddService.serviceName()
      println(serviceName)
    }
    case (SpecTypeSwagger, parsedSpec) => {
      val swaggerService = new ServiceSwagger(parsedSpec)
      val serviceName = swaggerService.serviceName()
      println(serviceName)
    }
    case (_, _) => {
      println("Invalid Input")
    }
  }
}
