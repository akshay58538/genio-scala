package com.paypal.genio

import scala.collection.immutable.HashMap

/**
 * Created by akgoel on 03/07/15.
 */

class SpecGDD(parsedSpec: Map[String, Any]) extends ServiceSpec with ServiceSpecProcessor{
  override def processServiceName(): Unit = name = readMapEntity("name").get

  override def processServiceRootUrl(): Unit = rootUrl = readMapEntity("rootUrl").get

  override def processServiceBasePath(): Unit = basePath = if (readMapEntity("basePath") != None) readMapEntity("basePath").get else readMapEntity("servicePath").get

  override def processResources(): Unit ={
    val resourcesMap = resourcesMapRef()
    resourcesMap.foreach{
      case (resourceName, resourceMap) => {
        processResource(resourceMap.asInstanceOf[Map[String, Any]], ResourceRefTypeCore, resourceName)
      }
    }
  }

  def processResource(resourceMap:Map[String, Any], resourceRefType: ResourceRefType, resourceName:ResourceKey): ResourceKey ={
    val resource = new Resource("/")
    val methodsMap = Utils.readMapEntity[Map[String, Any]](resourceMap.asInstanceOf[Map[String, Any]], "methods").get
    methodsMap.foreach {
      case (methodName, methodMap) => {
        val method = processMethod(methodMap.asInstanceOf[Map[String, Any]])
        resource.addMethod(methodName, method)
      }
    }
    val subResourcesMap = Utils.readMapEntity[Map[String, Any]](resourceMap.asInstanceOf[Map[String, Any]], "resources").get
    subResourcesMap.foreach {
      case (subResourceName, subResourceMap) => {
        resource.addSubResource(processResource(subResourceMap.asInstanceOf[Map[String, Any]], ResourceRefTypeSub, subResourceName))
      }
    }
    val resourceKey = Utils.keyForResourceRef(resourceRefType, resourceName)
    addResource(resourceKey, resource)
    resourceKey
  }

  def processMethod(methodMap:Map[String, Any]): Method = {
    val path = Utils.readMapEntity[String](methodMap, "path").get
    val httpMethod = Mapper.httpMethod(Utils.readMapEntity[String](methodMap, "httpMethod").get)
    val method = new Method(path, httpMethod)
    val parameterMap = Utils.readMapEntity[Map[String, Any]](methodMap, "parameters").getOrElse(new HashMap[String, Any])
    parameterMap.foreach{
      case (parameterName, parameterMap) => {
        val parameterKey = processSubSchema(parameterMap.asInstanceOf[Map[String, Any]], SchemaRefTypeParameter, parameterName)
        method.addParameter(parameterName, parameterKey)
      }
    }
    parametersMapRef().keySet.foreach {
      case parameterName => {
        method.addParameter(parameterName, Utils.keyForSchemaRef(SchemaRefTypeParameter, parameterName))
      }
    }
    val responseMap = Utils.readMapEntity[Map[String, Any]](methodMap, "response")
    httpMethod match {
      case HttpGet => method.addResponse(200, processMethodResponse(responseMap).orNull)
      case HttpPost => method.addResponse(201, processMethodResponse(responseMap).orNull)
      case HttpDelete => method.addResponse(200, processMethodResponse(responseMap).orNull)
      case HttpPut => method.addResponse(200, processMethodResponse(responseMap).orNull)
      case HttpPatch => method.addResponse(200, processMethodResponse(responseMap).orNull)
      case HttpOptions => method.addResponse(200, processMethodResponse(responseMap).orNull)
      case HttpMethodInvalid =>
    }
    val requestMap = Utils.readMapEntity[Map[String, Any]](methodMap, "request")
    method.request = processMethodRequest(requestMap).orNull
    method.id = Utils.readMapEntity(methodMap, "id").orNull
    method
  }

  def processMethodRequest(requestMap:Option[Map[String, Any]]) : Option[SchemaKey] ={
    requestMap match {
      case Some(map) => Option(processSubSchema(map, SchemaRefTypeCore, null))
      case None => None
    }
  }

  def processMethodResponse(responseMap:Option[Map[String, Any]]): Option[SchemaKey] = {
    responseMap match {
      case Some(map) => Option(processSubSchema(map.asInstanceOf[Map[String, Any]], SchemaRefTypeCore, null))
      case None => None
    }
  }

  override def processParameter(parameterMap: Map[String, Any], schemaRefType: SchemaRefType, parameterName: String) ={
    processSubSchema(parameterMap, schemaRefType, parameterName)
  }

  override def schemaMapRef():Map[String, Any] ={
    readMapEntity[Map[String,Any]]("schemas").get
  }

  override def resourcesMapRef():Map[String, Any] ={
    readMapEntity[Map[String, Any]]("resources").get
  }

  override var spec = parsedSpec
  process()
}

class SpecSwagger(parsedSpec: Map[String, Any]) extends ServiceSpec with ServiceSpecProcessor{
  override def processServiceName(): Unit = name = readMapEntity("info.title").get

  override def processServiceRootUrl(): Unit = rootUrl = readMapEntity("host").get

  override def processServiceBasePath(): Unit = basePath = readMapEntity("basePath").get

  override def processResources(): Unit = {

  }

  override def processParameter(map: Map[String, Any], schemaRefType: SchemaRefType, parameterName: String) ={

  }

  override def schemaMapRef():Map[String, Any] ={
    readMapEntity[Map[String, Any]]("definitions").get
  }

  override def resourcesMapRef():Map[String, Any] ={
    readMapEntity[Map[String, Any]]("paths").get
  }

  override var spec = parsedSpec
  process()
}