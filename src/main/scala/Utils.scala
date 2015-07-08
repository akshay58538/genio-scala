package com.paypal.genio

import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
 * Created by akgoel on 07/07/15.
 */
object Utils {
  /*
    accessKey can be used to access multiple levels into the map.
    For eg: "info.contact.name", "paths./pets.post.parameters#0.name" etc
    Use dot(.) notation to access sub-objects and hash(#) to access arrays
   */
  def readMapEntity[T](map:Map[String, Any], accessKey:String): Option[T] = {
    val keyIterator = accessKey.split("""\.""").toIterator
    try {
      readMapEntity[T](map, keyIterator)
    } catch {
      case _:Throwable => None
    }
  }

  def readMapEntity[T](map:Map[String, Any], keyIterator:Iterator[String]): Option[T] = {
    val key = keyIterator.next()
    key match {
      case variableKey if variableKey.contains("#") => {
        val arrayKeyIterator = variableKey.split("#").iterator
        val arrayKey = arrayKeyIterator.next()
        val arrayIndex = arrayKeyIterator.next().toInt
        if(keyIterator.hasNext)
          readMapEntity[T](readMapArrayEntity[Map[String, Any]](map, arrayKey, arrayIndex).get, keyIterator)
        else
          readMapArrayEntity[T](map, arrayKey, arrayIndex)
      }
      case simpleKey => {
        val value = map.get(simpleKey).get
        value match {
          case m:T => Option(value.asInstanceOf[T])
          case m: Map[String, Any] => readMapEntity[T](m, keyIterator)
          case _ => Option(value.asInstanceOf[T])
        }
      }
    }
  }

  def readMapArrayEntity[T](map:Map[String, Any], arrayKey:String, index:Int): Option[T] = {
    try{
      Option(map.get(arrayKey).get.asInstanceOf[List[T]](index))
    } catch {
      case _:Throwable => None
    }
  }
}

object SchemaUtils{
  def resolveSchemaRefs(map:Map[String, Any], schema:Map[String, Any]): Option[Map[String, Any]] ={
    var mSchema = new mutable.HashMap[String, Any] ++ schema
    val keyRef = "$ref"
    mSchema.foreach {
      case (k, v) => {
        if (k == keyRef) {
          val referredSchema = search(map, v.asInstanceOf[String])
          referredSchema match {
            case Some(value) => {
              mSchema.remove(k)
              mSchema ++= value
            }
            case None => //Raise invalid schema ref exception here
          }
        } else {
          v match {
            case m:Map[String, Any] => mSchema.put(k, resolveSchemaRefs(map, m).get)
            case _ => v
          }
        }
      }
    }
    Option(new HashMap[String, Any] ++ mSchema)
  }

  def searchSchema (map:Map[String, Any], schemaKey:SchemaKey): Option[Map[String, Any]] ={
    map.foreach{ case (k, v) =>
      if (k == schemaKey) {
        return Utils.readMapEntity(map, schemaKey)
      } else {
        v match {
          case m: Map[String, Any] => searchSchema(m, schemaKey)
          case _ =>
        }
      }
    }
    None
  }

  def search(schemaMap:Map[String,Any], schemaKey:SchemaKey): Option[Map[String, Any]] ={
    val schema = searchSchema(schemaMap, schemaKey).get
    resolveSchemaRefs(schemaMap, schema)
  }
}
