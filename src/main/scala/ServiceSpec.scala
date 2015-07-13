package com.paypal.genio

import scala.collection.mutable
import scala.reflect.ClassTag

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

sealed abstract class Reference
case object ReferenceInternal extends Reference
case object ReferenceExternalFile extends Reference
case object ReferenceExternalURL extends Reference

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

  def readMapEntity[T:ClassTag](key:String) = {
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
    val schemaType:SchemaType = Mapper.schemaType(typeString.getOrElse("default"))
    val schema = new Schema(schemaType)
    val format:FormatType = Mapper.formatType(Utils.readMapEntity(schemaMap, "format").getOrElse("default"))
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
      case SchemaTypeInteger => handleFormatForInteger(format, schema)
      case SchemaTypeNumber => handleFormatForNumber(format, schema)
      case SchemaTypeString => handleFormatForString(format, schema)
      case SchemaTypeBoolean => // Do nothing
      case SchemaTypeInvalid => //Raise invalid schema definition - Invalid Schema Type
    }
    processCommonSchemaFields(schemaMap, schema)
    schema
  }

  private def handleFormatForInteger(format:FormatType, schema: Schema): Unit ={
    format match {
      case FormatTypeInt32 => schema.format = FormatTypeInt32
      case FormatTypeUInt32 => schema.format = FormatTypeUInt32
      case FormatTypeInvalid => //No format specified - Do nothing
      case _ => //Raise invalid schema definition - Invalid format for type
    }
  }

  private def handleFormatForNumber(format:FormatType, schema: Schema): Unit ={
    format match {
      case FormatTypeDouble => schema.format = FormatTypeDouble
      case FormatTypeFloat => schema.format = FormatTypeFloat
      case FormatTypeInvalid => //No format specified - Do nothing
      case _ => //Raise invalid schema definition - Invalid format for type
    }
  }

  private def handleFormatForString(format:FormatType, schema:Schema): Unit ={
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

  private def processCommonSchemaFields(schemaMap:Map[String, Any], schema:Schema) ={
    schema.id = Utils.readMapEntity[String](schemaMap, "id").orNull
    schema.location = Mapper.schemaLocation(Utils.readMapEntity[String](schemaMap, "location").getOrElse("default"))
    schema.description = Utils.readMapEntity[String](schemaMap, "description").orNull
//    schema.required = Utils.readMapEntity[Boolean](schemaMap, "required").getOrElse(false)
    schema.enum = Utils.readMapEntity[List[Any]](schemaMap, "enum").orNull
  }

  private def processSubSchema(subSchemaMap:Map[String, Any]):Schema ={
    if(containsSchemaRef(subSchemaMap)){
      val (referenceType,referred) = processRef(Utils.readMapEntity[String](subSchemaMap, "$ref").get)
      var referredSchema:Schema = null
      if(schemas.keySet.contains(referred)){
        referredSchema = getSchema(referred).get
      } else {
        var schema:Option[Map[String, Any]] = null
        referenceType match {
          case ReferenceInternal => schema = Utils.readMapEntity[Map[String, Any]](schemaMapRef(), referred)
          case ReferenceExternalFile => schema = externalFileSchema(referred)
          case ReferenceExternalURL => schema = externalWebSchema(referred)
          case _ =>
        }
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

  private def externalWebSchema(referred:String):Option[Map[String,Any]]= {
    val r = new Reader()
    val content =r.readWebUrl(referred)
    Option(r.parser(referred,content))
  }

  private def externalFileSchema(referred:String):Option[Map[String, Any]]  = {
    val r = new Reader()
    val content =r.readFile("/" + referred)
    Option(r.parser(referred,content))
  }

  private def containsSchemaRef(map:Map[String, Any]):Boolean ={
    map.keySet.contains("$ref")
  }

  private def processRef(ref:String):(Reference,String) ={
    ref match {
      case internal if ref.startsWith("#") => (ReferenceInternal,ref.split("""/""").reverseIterator.next())
      case externalurl if ref.startsWith("http") => (ReferenceExternalURL,ref)
      case externalfile if ref.endsWith(".json")|| ref.endsWith(".yaml") => (ReferenceExternalFile,ref)
      case _ => (ReferenceInternal,ref)
    }
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
    case _ => SchemaTypeObject        //Default schema type
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
