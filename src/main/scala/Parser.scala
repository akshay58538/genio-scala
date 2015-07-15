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
        val resource = processResource(resourceMap.asInstanceOf[Map[String, Any]])
        addResource(resourceName, resource)
      }
    }
  }

  def processResource(resourceMap:Map[String, Any]): Resource ={
    val resource = new Resource("/")
    val methodsMap = Utils.readMapEntity[Map[String, Any]](resourceMap.asInstanceOf[Map[String, Any]], "methods").get
    methodsMap.foreach {
      case (methodName, methodMap) => {
        val method = processMethod(methodMap.asInstanceOf[Map[String, Any]])
        resource.addMethod(methodName, method)
      }
    }
    resource
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

  override def processResources(): Unit ={
    val resourcesMap = resourcesMapRef()
    resourcesMap.foreach{
      case (resourcePath, resourceMap) => {
        val (resourceName, resource) = processResource(resourcePath.stripPrefix("/").stripSuffix("/").trim, resourceMap.asInstanceOf[Map[String, Any]], resourcePath, null)
        addResource(resourceName, resource)
      }
    }
  }

  def processResource(resourcePath:String, resourceMap:Map[String, Any], restPath:String, parentResource:Resource): (String, Resource) ={
    val resourceName = "/" + resourcePath.split("/",2)(0)
    var resource:Resource = null
    if (parentResource == null) {
      getResource(resourceName) match {
        case Some(r) => resource = getResource(resourceName).get
        case None => resource = new Resource(resourceName)
      }
    } else {
      parentResource.getResource(resourceName) match {
        case Some(r) => resource = parentResource.getResource(resourceName).get
        case None => resource = new Resource(resourceName)
      }
    }
    if (resourcePath.split("/",2).length>1) {
      val (subResourceName, subResource) = processResource(resourcePath.split("/",2)(1), resourceMap.asInstanceOf[Map[String, Any]], restPath, resource)
      resource.addSubResource(subResourceName,subResource)
    } else {
      resourceMap.foreach {
        case (methodName, methodMap) => {
          val method = processMethod(methodName, methodMap.asInstanceOf[Map[String, Any]], restPath)
          resource.addMethod(methodName, method)
        }
      }
    }
    (resourceName, resource)
  }

  def processMethod(methodName: String, methodMap: Map[String, Any], resourcePath:String): Method = {
    val path = resourcePath;
    val httpMethod = Mapper.httpMethod(methodName.toUpperCase)
    val method = new Method(path, httpMethod)
    val parameterList = Utils.readMapEntity[List[Map[String, Any]]](methodMap, "parameters").getOrElse(List())
    parameterList.foreach{
      parameter => {
        if (parameter.get("in").get.asInstanceOf[String] == "body") {
          method.request = processMethodRequest(Option(parameter)).orNull
          method.id = Utils.readMapEntity(methodMap, "id").orNull
        } else {
          val parameterName = parameter.get("name").get.asInstanceOf[String]
          val parameterMap = processSubSchema(parameter, SchemaRefTypeParameter, parameterName)
          method.addParameter(parameterName, parameterMap)
        }
      }
    }
    val responses = Utils.readMapEntity[Map[String, Any]](methodMap, "responses").get
    responses.foreach{
      case (statusCode:String, responseMap:Map[String,Any]) => {
        if (statusCode != "default") {              //TODO: Default in status codes is yet to be mapped with a status code
          method.addResponse(statusCode.toInt, processMethodResponse(Option(responseMap)).orNull)
        }
      }
    }
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
      case Some(map) => Option(processSubSchema(map, SchemaRefTypeCore, null))
      case None => None
    }
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