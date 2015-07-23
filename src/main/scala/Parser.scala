package com.paypal.genio

import scala.collection.immutable.HashMap
import scala.collection.immutable.Map

/**
 * Created by akgoel on 03/07/15.
 */

class SpecGDD(parsedSpec: Map[String, Any]) extends ServiceSpec with ServiceSpecProcessor{
  override def processServiceName(): Unit = name = readMapEntity("name").get

  override def processServiceRootUrl(): Unit = rootUrl = readMapEntity("rootUrl").get

  override def processServiceBasePath(): Unit = basePath = if (readMapEntity("basePath") != None) readMapEntity("basePath").get else readMapEntity("servicePath").get

  override def processPaths(): Unit ={
    val resourcesMap = readMapEntity[Map[String, Any]]("resources").get
    resourcesMap.foreach{
      case (resourceName, resourceMap) => {
        processResource(resourceMap.asInstanceOf[Map[String, Any]])
      }
    }
  }

  def processResource(resourceMap:Map[String, Any]): Unit ={
    val methodsMap = Utils.readMapEntity[Map[String, Any]](resourceMap.asInstanceOf[Map[String, Any]], "methods").get
    methodsMap.foreach {
      case (methodName, methodMap) => processPath(Utils.readMapEntity(methodMap.asInstanceOf[Map[String, Any]], "path").getOrElse(throw new Exception("Missing method path")), methodMap.asInstanceOf[Map[String, Any]], paths)
    }
    val subResourcesMap = Utils.readMapEntity[Map[String, Any]](resourceMap.asInstanceOf[Map[String, Any]], "resources").getOrElse(new HashMap[String, Any]())
    subResourcesMap.foreach {
      case (subResourceName, subResourceMap) => {
        processResource(subResourceMap.asInstanceOf[Map[String, Any]])
      }
    }
  }

  def processPath(methodPath:String, methodMap:Map[String, Any], processedPaths:collection.mutable.Map[PathKey, Path]): Unit ={
    val pathArray = methodPath.stripPrefix("/").stripSuffix("/").trim.split("/", 2)
    if(pathArray.length == 0){
      throw new Exception("Invalid Path Spec Found")
    } else {
      val currentPath = pathArray(0)
      val processedPath = processedPaths.get(currentPath).getOrElse(new Path())
      if(pathArray.length == 1){
        val method = processMethod(methodMap.asInstanceOf[Map[String, Any]])
        val methodName = Utils.readMapEntity[String](methodMap, "httpMethod").get.toLowerCase + currentPath.charAt(0).toUpper + currentPath.substring(1)
        processedPath.addMethod(methodName, method)
      } else if(pathArray.length == 2){
        val unProcessedPath = pathArray(1)
        processPath(unProcessedPath, methodMap, processedPath.subPaths)
      }
      processedPaths.put(currentPath, processedPath)
    }
  }

  def processMethod(methodMap:Map[String, Any]): Method = {
    val httpMethod = Mapper.httpMethod(Utils.readMapEntity[String](methodMap, "httpMethod").get)
    val method = new Method(httpMethod)
    val parameterMap = Utils.readMapEntity[Map[String, Any]](methodMap, "parameters").getOrElse(new HashMap[String, Any])
    parameterMap.foreach{
      case (parameterName, parameterMap) => {
        val parameterKey = processSubSchema(parameterMap.asInstanceOf[Map[String, Any]], SchemaRefTypeParameter, parameterName)
        method.addParameter(parameterName, parameterKey)
      }
    }
    parametersMapRef().keySet.foreach {
      case parameterName => method.addParameter(parameterName, Utils.keyForSchemaRef(SchemaRefTypeParameter, parameterName))
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

  override var spec = parsedSpec
  process()
}

class SpecSwagger(parsedSpec: Map[String, Any]) extends ServiceSpec with ServiceSpecProcessor{
  override def processServiceName(): Unit = name = readMapEntity("info.title").get

  override def processServiceRootUrl(): Unit = rootUrl = readMapEntity("host").get

  override def processServiceBasePath(): Unit = basePath = readMapEntity("basePath").get

  override def processPaths(): Unit ={
    val pathsMap = readMapEntity[Map[String, Any]]("paths").get
    pathsMap.foreach{
      case (path, pathMap) => {
        processPath(path, pathMap.asInstanceOf[Map[String, Any]], paths)
      }
    }
  }

  def processPath(path:String, pathMap:Map[String, Any], processedPaths:collection.mutable.Map[PathKey, Path]): Unit ={
    val pathArray = path.stripPrefix("/").stripSuffix("/").trim.split("/", 2)
    if(pathArray.length == 0){
      throw new Exception("Invalid Path Spec Found")
    } else {
      val currentPath = pathArray(0)
      val processedPath = processedPaths.get(currentPath).getOrElse(new Path())
      if(pathArray.length == 1){
        pathMap.foreach{
          case (methodType, methodMap) => {
            val method = processMethod(methodType, methodMap.asInstanceOf[Map[String, Any]])
            val methodName = Utils.readMapEntity(methodMap.asInstanceOf[Map[String, Any]], "operationId").getOrElse(methodType + currentPath.charAt(0).toUpper + currentPath.substring(1))
            processedPath.addMethod(methodName, method)
          }
        }
      } else if(pathArray.length == 2){
        val unProcessedPath = pathArray(1)
        processPath(unProcessedPath, pathMap, processedPath.subPaths)
      }
      processedPaths.put(currentPath, processedPath)
    }
  }

  def processMethod(methodType: String, methodMap: Map[String, Any]): Method = {
    val httpMethod = Mapper.httpMethod(methodType.toUpperCase)
    val method = new Method(httpMethod)
    val parameterList = Utils.readMapEntity[List[Map[String, Any]]](methodMap, "parameters").getOrElse(List())
    parameterList.foreach{
      parameter => {
        if (parameter.get("in").get.asInstanceOf[String] == "body") {
          method.request = processMethodRequest(Option(parameter)).orNull
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

  override def processParameter(map: Map[String, Any], schemaRefType: SchemaRefType, parameterName: String) ={

  }

  override def schemaMapRef():Map[String, Any] ={
    readMapEntity[Map[String, Any]]("definitions").get
  }

  override var spec = parsedSpec
  process()
}