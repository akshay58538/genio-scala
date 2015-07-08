package com.paypal.genio

import scala.collection.mutable

/**
 * Created by akgoel on 03/07/15.
 */

sealed abstract class SchemaType
case object SchemaTypeString extends SchemaType
case object SchemaTypeNumber extends SchemaType
case object SchemaTypeInteger extends SchemaType
case object SchemaTypeBoolean extends SchemaType
case object SchemaTypeObject extends SchemaType
case object SchemaTypeArray extends SchemaType
case object SchemaTypeInvalid extends SchemaType

sealed abstract class FormatType
case object FormatTypeInt32 extends FormatType
case object FormatTypeUInt32 extends FormatType
case object FormatTypeDouble extends FormatType
case object FormatTypeFloat extends FormatType
case object FormatTypeByte extends FormatType
case object FormatTypeDate extends FormatType
case object FormatTypeDateTime extends FormatType
case object FormatTypeInt64 extends FormatType
case object FormatTypeUInt64 extends FormatType
case object FormatTypeInvalid extends FormatType

sealed abstract class SchemaLocation
case object SchemaLocationQuery extends SchemaLocation
case object SchemaLocationPath extends SchemaLocation
case object SchemaLocationInvalid extends SchemaLocation

sealed abstract class HttpMethod
case object HttpGet extends HttpMethod
case object HttpPost extends HttpMethod
case object HttpPut extends HttpMethod
case object HttpDelete extends HttpMethod
case object HttpPatch extends HttpMethod
case object HttpOptions extends HttpMethod
case object HttpMethodInvalid extends HttpMethod

class Schema(
              var schemaType:SchemaType,
              var format:FormatType = null,
              var location:SchemaLocation = null,
              var description:String = "",
              var required:Boolean = false,
              var id:String = "",
              var enum:List[Any] = null,
              val properties:mutable.Map[String, Property] = new mutable.HashMap[String, Property](),
              var items:Schema = null
              ){
  def getProperty(propertyName:String): Option[Property] ={
    properties.get(propertyName)
  }

  def addProperty(propertyName:String, property: Property): Unit ={
    properties.put(propertyName, property)
  }

  def removeProperty(propertyName:String) ={
    properties.remove(propertyName)
  }
}

class Method(
              var path:String,
              var httpMethod: HttpMethod,
              var id:String = "",
              val parameters:mutable.Map[String, Parameter] = new mutable.HashMap[String, Parameter](),
              var request:Schema = null,
              val responses:mutable.Map[Int, Schema] = new mutable.HashMap[Int, Schema]()
              ){
  def addParameter(name:String, parameter: Parameter): Unit ={
    parameters.put(name, parameter)
  }

  def getParameter(name: String): Option[Parameter] ={
    parameters.get(name)
  }

  def removeParameter(name:String): Unit ={
    parameters.remove(name)
  }

  def addResponse(statusCode:Int, response:Schema): Unit ={
    responses.put(statusCode, response)
  }

  def getResponse(statusCode:Int): Option[Schema] ={
    responses.get(statusCode)
  }

  def removeResponse(statusCode:Int): Unit ={
    responses.remove(statusCode)
  }
}

class Resource(
                var path:String,
                val resources:mutable.Map[ResourceKey, Resource] = new mutable.HashMap[ResourceKey, Resource](),
                val methods:mutable.Map[MethodKey, Method] = new mutable.HashMap[MethodKey, Method]()
                ){
  def addSubResource(resourceKey: ResourceKey, resource: Resource): Unit ={
    resources.put(resourceKey, resource)
  }

  def getResource(resourceKey: ResourceKey): Option[Resource] ={
    resources.get(resourceKey)
  }

  def removeResource(resourceKey: ResourceKey): Unit ={
    resources.remove(resourceKey)
  }

  def addMethod(methodKey: MethodKey, method: Method): Unit ={
    methods.put(methodKey, method)
  }

  def getMethod(methodKey: MethodKey): Option[Method] ={
    methods.get(methodKey)
  }

  def removeMethod(methodKey: MethodKey): Unit ={
    methods.remove(methodKey)
  }
}

trait ServiceSpec{
  var name:String = null
  var basePath:String = null
  var rootUrl:String = null
  val schemas:mutable.Map[SchemaKey, Schema] = new mutable.HashMap[SchemaKey, Schema]()
  val resources:mutable.Map[ResourceKey, Resource] = new mutable.HashMap[ResourceKey, Resource]()

  def getSchema(schemaKey: SchemaKey): Option[Schema] ={
    schemas.get(schemaKey)
  }

  def addSchema(schemaKey: SchemaKey, schema:Schema) ={
    schemas.put(schemaKey, schema)
  }

  def removeSchema(schemaKey: SchemaKey) ={
    schemas.remove(schemaKey)
  }

  def getResource(resourceKey: ResourceKey): Option[Resource] ={
    resources.get(resourceKey)
  }

  def addResource(resourceKey: ResourceKey, resource: Resource) ={
    resources.put(resourceKey, resource)
  }

  def removeResource(resourceKey: ResourceKey) ={
    resources.remove(resourceKey)
  }
}

object Mapper {
  def formatType(format:String) = format match {
    case "int32" => FormatTypeInt32
    case "uint32" => FormatTypeUInt32
    case "double" => FormatTypeDouble
    case "float" => FormatTypeFloat
    case "byte" => FormatTypeByte
    case "date" => FormatTypeDate
    case "date-time" => FormatTypeDateTime
    case "int64" => FormatTypeInt64
    case "uint64" => FormatTypeUInt64
    case _ => FormatTypeInvalid
  }

  def schemaType(schemaType:String) = schemaType match {
    case "string" => SchemaTypeString
    case "number" => SchemaTypeNumber
    case "integer" => SchemaTypeInteger
    case "boolean" => SchemaTypeBoolean
    case "object" => SchemaTypeObject
    case "array" => SchemaTypeArray
    case _ => SchemaTypeInvalid
  }

  def schemaLocation(schemaLocation:String) = schemaLocation match {
    case "path" => SchemaLocationPath
    case "query" => SchemaLocationQuery
    case _ => SchemaLocationInvalid
  }

  def httpMethod(httpMethod: String) = httpMethod match {
    case "GET" => HttpGet
    case "POST" => HttpPost
    case "PUT" => HttpPut
    case "DELETE" => HttpDelete
    case "PATCH" => HttpPatch
    case "OPTIONS" => HttpOptions
    case _ => HttpMethodInvalid
  }
}
