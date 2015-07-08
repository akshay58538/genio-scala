package com.paypal.genio

/**
 * Created by akgoel on 03/07/15.
 */

class SpecGDD(parsedSpec: Map[String, Any]) extends ServiceSpec with ServiceSpecProcessor{
  override def processServiceName(): Unit = name = readMapEntity("name").get

  override def processServiceRootUrl(): Unit = rootUrl = readMapEntity("rootUrl").get

  override def processServiceBasePath(): Unit = basePath = if (readMapEntity("basePath") != None) readMapEntity("basePath").get else readMapEntity("servicePath").get

  override def processSchemas(): Unit = {

  }

  override def processResources(): Unit = {

  }

  spec = parsedSpec
  process()
}

class SpecSwagger(parsedSpec: Map[String, Any]) extends ServiceSpec with ServiceSpecProcessor{
  override def processServiceName(): Unit = name = readMapEntity("info.title").get

  override def processServiceRootUrl(): Unit = rootUrl = readMapEntity("host").get

  override def processServiceBasePath(): Unit = basePath = readMapEntity("basePath").get

  override def processSchemas(): Unit = {

  }

  override def processResources(): Unit = {

  }

  spec = parsedSpec
  process()
}