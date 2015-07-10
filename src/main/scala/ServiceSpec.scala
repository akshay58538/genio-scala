package com.paypal.genio

import scala.collection.immutable.HashMap
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

trait ServiceSpecProcessor extends ServiceSpec{
  var spec: Map[String, Any]

  def readMapEntity[T](key:String) = {
    Utils.readMapEntity[T](spec, key)
  }

  def readArrayEntity[T](key:String, index:Int) = {
    Utils.readMapArrayEntity[T](spec, key, index)
  }

  def process() = {
    processServiceName()
    processServiceBasePath()
    processServiceRootUrl()
    processSchemas()
    processResources()
  }

  def processServiceName()
  def processServiceBasePath()
  def processServiceRootUrl()
  def processResources()
  def schemaMapRef():Map[String, Any]

  def processSchemas(): Unit = {
    val schemaMap:Map[String, Any] = schemaMapRef()
    schemaMap.foreach {
      case(key, value) => {
        val schema = getSchema(key)
        schema match {
          case Some(s) => //Schema already processed
          case None => {
            val processedSchema = processSchema(value.asInstanceOf[Map[String, Any]])
            if(!schemas.keySet.contains(key)){
              addSchema(key, processedSchema)
            }
          }
        }
      }
    }
  }

  def processSchema(schemaMap:Map[String, Any]): Schema ={
    val typeString = Utils.readMapEntity[String](schemaMap, "type")
    var schemaType:SchemaType = null
    typeString match {
      case Some(typeStr) => {
        schemaType = Mapper.schemaType(typeStr)
      }
      case None => schemaType = SchemaTypeObject
    }
    val schema = new Schema(schemaType)
    var format:FormatType = FormatTypeInvalid
    Utils.readMapEntity[String](schemaMap, "format") match {
      case Some(formatType) => {
        format = Mapper.formatType(formatType)
      }
      case None => //No format specified
    }
    schemaType match {
      case SchemaTypeArray => {
        val items = Utils.readMapEntity[Map[String, Any]](schemaMap, "items")
        items match {
          case Some(itemsVal) => {
            itemsVal match {
              case itemsValue:Map[String, Any] => {
                val itemsSchema = processSubSchema(itemsValue)
                schema.items = itemsSchema
              }
              case _ => //Raise invalid schema definition - Invalid array items
            }
          }
          case None => //Raise invalid schema definition - Missing array items
        }
      }
      case SchemaTypeObject => {
        val properties = Utils.readMapEntity[Map[String, Any]](schemaMap, "properties")
        properties match {
          case Some(propertiesVal) => {
            propertiesVal match {
              case propertiesMap:Map[String, Any] => {
                propertiesMap.foreach {
                  case (propertyName:String, propertyMap:Map[String, Any]) => {
                    schema.addProperty(propertyName, processSubSchema(propertyMap))
                  }
                }
              }
              case _ => //Raise invalid schema definiton - Invalid object properties
            }
          }
          case None => //Raise invalid schema definition - Missing object properties
        }
      }
      case SchemaTypeInteger => {
        format match {
          case FormatTypeInt32 => schema.format = FormatTypeInt32
          case FormatTypeUInt32 => schema.format = FormatTypeUInt32
          case FormatTypeInvalid => //No format specified - Do nothing
          case _ => //Raise invalid schema definition - Invalid format for type
        }
      }
      case SchemaTypeNumber => {
        format match {
          case FormatTypeDouble => schema.format = FormatTypeDouble
          case FormatTypeFloat => schema.format = FormatTypeFloat
          case FormatTypeInvalid => //No format specified - Do nothing
          case _ => //Raise invalid schema definition - Invalid format for type
        }
      }
      case SchemaTypeString => {
        format match {
          case FormatTypeByte => schema.format = FormatTypeByte
          case FormatTypeDate => schema.format = FormatTypeDate
          case FormatTypeDateTime => schema.format = FormatTypeDateTime
          case FormatTypeInt64 => schema.format = FormatTypeInt64
          case FormatTypeUInt64 => schema.format = FormatTypeUInt64
          case FormatTypeInvalid => //No format specified - Do nothing
          case _ => //Raise invalid schema definition - Invalid format for type
        }
      }
      case SchemaTypeBoolean => // Do nothing
      case SchemaTypeInvalid => //Raise invalid schema definition - Invalid Schema Type
    }
    processCommonSchemaFields(schemaMap, schema)
    schema
  }

  private def processCommonSchemaFields(schemaMap:Map[String, Any], schema:Schema) ={
    Utils.readMapEntity[String](schemaMap, "id") match {
      case Some(id) => {
        schema.id = id
      }
      case None => //No id specified - do nothing
    }
    Utils.readMapEntity[String](schemaMap, "location") match {
      case Some(location) => {
        schema.location = Mapper.schemaLocation(location)
      }
      case None => //No location specified - do nothing
    }
    Utils.readMapEntity[String](schemaMap, "description") match {
      case Some(description) => {
        schema.description = description
      }
      case None => //No location specified - do nothing
    }
    Utils.readMapEntity[Boolean](schemaMap, "required") match {
      case Some(required) => {
        schema.required = required
      }
      case None => //No location specified - do nothing
    }
    Utils.readMapEntity[List[Any]](schemaMap, "enum") match {
      case Some(enum) => {
        schema.enum = enum
      }
      case None => //No location specified - do nothing
    }
  }

  private def processSubSchema(subSchemaMap:Map[String, Any]):Schema ={
    if(containsSchemaRef(subSchemaMap)){
      val referred = Utils.readMapEntity[String](subSchemaMap, "$ref").get
      var referredSchema:Schema = null
      if(schemas.keySet.contains(referred)){
        referredSchema = getSchema(referred).get
      } else {
        val schema = Utils.readMapEntity[Map[String, Any]](schemaMapRef(), referred)
        schema match {
          case Some(schemaMap) => {
            referredSchema = processSchema(schemaMap)
            addSchema(referred, referredSchema)
          }
          case None => throw new Exception("Invalid Schema Definition - Invalid value $ref:" + referred)
        }
      }
      val propertyMapWithoutRef = subSchemaMap - "$ref"
      processCommonSchemaFields(propertyMapWithoutRef, referredSchema)
      referredSchema
    } else {
      processSchema(subSchemaMap)
    }
  }

  private def containsSchemaRef(map:Map[String, Any]):Boolean ={
    map.keySet.contains("$ref")
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

  override def toString: String = {
    "ServiceName: " + name + "\nServiceRoot: " + rootUrl + "\nServiceBasePath: " + basePath + "\nSchemas: " + schemas + "\nResources: " + resources
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
