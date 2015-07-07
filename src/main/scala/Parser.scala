package com.paypal.genio

import org.json4s.JsonAST.{JString, JObject, JValue}

/**
 * Created by akgoel on 03/07/15.
 */

trait Service {
  def serviceName(): String

  def servicePath(): String

  def serviceRoot(): String

  def schemas(): Map[String, Schema]

  def resources(): Map[String, Resource]
}

class ServiceGDD(parsedSpec: Map[String, Any]) extends Service {
  override def serviceName():String = parsedSpec.get("name").get.asInstanceOf[String]

  override def resources(): Map[String, Resource] = ???

  override def schemas(): Map[String, Schema] = ???

  override def servicePath(): String = Utils.readMapEntity(parsedSpec, "servicePath")

  override def serviceRoot(): String = Utils.readMapEntity(parsedSpec, "rootUrl")
}

class ServiceSwagger(parsedSpec: Map[String, Any]) extends Service {
  override def serviceName(): String = Utils.readMapEntity(parsedSpec, "info.title")

  override def resources(): Map[String, Resource] = ???

  override def schemas(): Map[String, Parameter] = ???

  override def servicePath():String = Utils.readMapEntity(parsedSpec, "basePath")

  override def serviceRoot():String = Utils.readMapEntity(parsedSpec, "host")
}