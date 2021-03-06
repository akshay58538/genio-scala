package com.paypal.genio

import org.json4s._
import org.json4s.native.Serialization._

import scala.collection.immutable.HashMap
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

sealed abstract class SchemaRefType
case object SchemaRefTypeCore extends SchemaRefType
case object SchemaRefTypeParameter extends SchemaRefType
case object SchemaRefTypeProperty extends SchemaRefType
case object SchemaRefTypeArrayItem extends SchemaRefType
case object SchemaRefTypeExternalFile extends SchemaRefType
case object SchemaRefTypeExternalURL extends SchemaRefType

class Schema(
              var schemaType:SchemaType,
              var format:FormatType = null,
              var location:SchemaLocation = null,
              var description:String = "",
              var required:Boolean = false,
              var id:String = "",
              var enum:List[Any] = null,
              val properties:mutable.Map[String, Property] = new mutable.HashMap[String, Property](),
              var items:SchemaKey = null
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

  override def toString():String ={
    implicit val formats = DefaultFormats
    write(this)
  }
}

class Method(
              var httpMethod: HttpMethod,
              val parameters:mutable.Map[String, Parameter] = new mutable.HashMap[String, Parameter](),
              var request:SchemaKey = null,
              val responses:mutable.Map[Int, SchemaKey] = new mutable.HashMap[Int, SchemaKey]()
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

  def addResponse(statusCode:Int, response:SchemaKey): Unit ={
    responses.put(statusCode, response)
  }

  def getResponse(statusCode:Int): Option[SchemaKey] ={
    responses.get(statusCode)
  }

  def removeResponse(statusCode:Int): Unit ={
    responses.remove(statusCode)
  }

  override def toString():String ={
    implicit val formats = DefaultFormats
    write(this)
  }
}

class Path(
            val subPaths:mutable.Map[PathKey, Path] = new mutable.HashMap[PathKey, Path](),
            val methods:mutable.Map[MethodKey, Method] = new mutable.HashMap[MethodKey, Method]()
            ){
  def addSubPath(pathKey: PathKey, path:Path): Unit ={
    subPaths.put(pathKey, path)
  }

  def getSubPath(pathKey: PathKey): Option[Path] ={
    subPaths.get(pathKey)
  }

  def removeSubPath(pathKey: PathKey): Unit ={
    subPaths.remove(pathKey)
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

  override def toString(): String ={
    implicit val formats = DefaultFormats
    write(this)
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
    processParameters()
    processPaths()
  }

  def processServiceName()
  def processServiceBasePath()
  def processServiceRootUrl()
  def processPaths()
  def schemaMapRef():Map[String, Any]
  def processParameter(map: Map[String, Any], schemaRefType: SchemaRefType, parameterName: String)

  def parametersMapRef():Map[String, Any] ={
    readMapEntity[Map[String, Any]]("parameters").getOrElse(new HashMap[String, Any]())
  }

  def processParameters() ={
    val parametersMap = parametersMapRef()
    parametersMap.foreach {
      case (parameterName, parameterMap) => {
        processParameter(parameterMap.asInstanceOf[Map[String, Any]], SchemaRefTypeParameter, parameterName)
      }
    }
  }

  def processSchemas(): Unit = {
    val schemaMap:Map[String, Any] = schemaMapRef()
    schemaMap.foreach {
      case(key, value) => {
        val schema = getSchema(Utils.keyForSchemaRef(SchemaRefTypeCore, key))
        schema match {
          case Some(s) => //Schema already processed
          case None => {
            processSchema(value.asInstanceOf[Map[String, Any]], SchemaRefTypeCore, key)
          }
        }
      }
    }
  }

  def processSchema(schemaMap:Map[String, Any], schemaRefType:SchemaRefType, suggestedKey:SchemaKey): SchemaKey ={
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
              case itemsValue:Map[String, Any] => schema.items = processSubSchema(itemsValue, SchemaRefTypeArrayItem, suggestedKey)
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
                    val propertyKey = suggestedKey + "-" + propertyName
                    schema.addProperty(propertyName, processSubSchema(propertyMap, SchemaRefTypeProperty, propertyKey))
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
    val schemaKey:SchemaKey = Utils.keyForSchemaRef(schemaRefType, suggestedKey)
    addSchema(schemaKey, schema)
    processCommonSchemaFields(schemaMap, schemaKey)
    schemaKey
  }

  def handleFormatForInteger(format:FormatType, schema: Schema): Unit ={
    format match {
      case FormatTypeInt32 => schema.format = FormatTypeInt32
      case FormatTypeUInt32 => schema.format = FormatTypeUInt32
      case FormatTypeInvalid => //No format specified - Do nothing
      case _ => //Raise invalid schema definition - Invalid format for type
    }
  }

  def handleFormatForNumber(format:FormatType, schema: Schema): Unit ={
    format match {
      case FormatTypeDouble => schema.format = FormatTypeDouble
      case FormatTypeFloat => schema.format = FormatTypeFloat
      case FormatTypeInvalid => //No format specified - Do nothing
      case _ => //Raise invalid schema definition - Invalid format for type
    }
  }

  def handleFormatForString(format:FormatType, schema:Schema): Unit ={
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

  def processCommonSchemaFields(schemaMap:Map[String, Any], schemaKey:SchemaKey) ={
    val schema:Schema = getSchema(schemaKey).get
    schema.id = Utils.readMapEntity[String](schemaMap, "id").orNull
    schema.location = Mapper.schemaLocation(Utils.readMapEntity[String](schemaMap, "location").getOrElse("default"))
    schema.description = Utils.readMapEntity[String](schemaMap, "description").orNull
//    schema.required = Utils.readMapEntity[Boolean](schemaMap, "required").getOrElse(false)
    schema.enum = Utils.readMapEntity[List[Any]](schemaMap, "enum").orNull
  }

  def processSubSchema(subSchemaMap:Map[String, Any], schemaRefType: SchemaRefType, suggestedKey:SchemaKey):SchemaKey ={
    if(containsSchemaRef(subSchemaMap)){
      val (referenceType, referred) = processRef(Utils.readMapEntity[String](subSchemaMap, "$ref").get)
      val schemaKey = Utils.keyForSchemaRef(referenceType, referred)
      if(!schemas.keySet.contains(schemaKey)){
        referenceType match {
          case SchemaRefTypeCore => processSchema(Utils.readMapEntity[Map[String, Any]](schemaMapRef(), referred).get, SchemaRefTypeCore, referred)
          case SchemaRefTypeExternalFile => processSchema(parseFileSchema(referred).get, SchemaRefTypeExternalFile, referred)
          case SchemaRefTypeExternalURL => processSchema(parseWebSchema(referred).get, SchemaRefTypeExternalURL, referred)
          case _ => throw new Exception("Invalid Schema Definition - Invalid value $ref:" + referred)
        }
      }
      processCommonSchemaFields(subSchemaMap, schemaKey)
      schemaKey
    } else {
      processSchema(subSchemaMap, schemaRefType, suggestedKey)
    }
  }

  private def parseWebSchema(referred:String):Option[Map[String,Any]]= {
    val r = new Reader()
    val content = r.readWebUrl(referred)
    Option(r.parser(referred,content))
  }

  private def parseFileSchema(referred:String):Option[Map[String, Any]]  = {
    val r = new Reader()
    val content = r.readFile("/" + referred)
    Option(r.parser(referred,content))
  }

  def containsSchemaRef(map:Map[String, Any]):Boolean ={
    map.keySet.contains("$ref")
  }

  def processRef(ref:String):(SchemaRefType, String) ={
    ref match {
      case internal if ref.startsWith("#") => (SchemaRefTypeCore, internal.split('/').reverseIterator.next())
      case externalUrl if ref.startsWith("http") => (SchemaRefTypeExternalURL, externalUrl)
      case externalFile if ref.endsWith(".json") || ref.endsWith(".yaml") => (SchemaRefTypeExternalFile, externalFile)
      case _ => (SchemaRefTypeCore, ref)
    }
  }
}

trait ServiceSpec{
  var name:String = null
  var basePath:String = null
  var rootUrl:String = null
  val schemas:mutable.Map[SchemaKey, Schema] = new mutable.HashMap[SchemaKey, Schema]()
  val paths:mutable.Map[PathKey, Path] = new mutable.HashMap[PathKey, Path]()

  def getSchema(schemaKey: SchemaKey): Option[Schema] ={
    schemas.get(schemaKey)
  }

  def addSchema(schemaKey: SchemaKey, schema:Schema) ={
    schemas.put(schemaKey, schema)
  }

  def removeSchema(schemaKey: SchemaKey) ={
    schemas.remove(schemaKey)
  }

  def getPath(pathKey: PathKey): Option[Path] ={
    paths.get(pathKey)
  }

  def addResource(pathKey: PathKey, path:Path) ={
    paths.put(pathKey, path)
  }

  def removePath(pathKey: PathKey) ={
    paths.remove(pathKey)
  }

  override def toString: String = {
    implicit val formats = DefaultFormats
    "ServiceName: " + name + "\nServiceRoot: " + rootUrl + "\nServiceBasePath: " + basePath + "\nSchemas: " + write(schemas) + "\nPaths: " + write(paths)
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
