package com.paypal.genio
/**
 * Created by akgoel on 07/07/15.
 */
object Utils {
  /*
    accessKey can be used to access multiple levels into the map.
    For eg: "info.contact.name", "paths./pets.post.parameters#0.name" etc
    Use dot(.) notation to access sub-objects and hash(#) to access arrays
   */
  def readMapEntity[T](map:Map[String, Any], accessKey:String) : T = {
    val keyIterator = accessKey.split("""\.""").toIterator
    readMapEntity[T](map, keyIterator)
  }

  def readMapEntity[T](map:Map[String, Any], keyIterator:Iterator[String]) : T = {
    val key = keyIterator.next()
    key match {
      case variableKey if variableKey.contains("#") => {
        val arrayKeyIterator = variableKey.split("#").iterator
        val arrayKey = arrayKeyIterator.next()
        val arrayIndex = arrayKeyIterator.next().toInt
        if(keyIterator.hasNext)
          readMapEntity[T](readMapArrayEntity[Map[String, Any]](map, arrayKey, arrayIndex), keyIterator)
        else
          readMapArrayEntity[T](map, arrayKey, arrayIndex)
      }
      case simpleKey => {
        val value = map.get(simpleKey).get
        value match {
          case m: Map[String, Any] => readMapEntity[T](m, keyIterator)
          case _ => value.asInstanceOf[T]
        }
      }
    }
  }

  def readMapArrayEntity[T](map:Map[String, Any], arrayKey:String, index:Int) : T = map.get(arrayKey).get.asInstanceOf[List[T]](index)
}
