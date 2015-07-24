package com.paypal.genio

/**
 * Created by akgoel on 03/07/15.
 */
object Main extends App {
  var r = new Reader()
  r.readSpec("/swagger.json") match {
    case (SpecTypeGDD, parsedSpec) => {
      val gddService:ServiceSpec = new SpecGDD(parsedSpec)
      println(gddService)
    }
    case (SpecTypeSwagger, parsedSpec) => {
      val swaggerService:ServiceSpec = new SpecSwagger(parsedSpec)
      println(swaggerService)
    }
    case (_, _) => {
      println("Invalid Input")
    }
  }
}
