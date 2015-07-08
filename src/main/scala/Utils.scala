package com.paypal.genio
/**
 * Created by akgoel on 07/07/15.
 */
object Utils {
  def readMapEntity[T](map:Map[String, Any], accessKey:String) : T = {
    val keyIterator = accessKey.split("""\.""").toIterator
    readMapEntity[T](map, keyIterator)
  }

  def readMapEntity[T](map:Map[String, Any], keyIterator:Iterator[String]) : T = {
    val key = keyIterator.next()
    println(key)
    key match {
      case variableKey if variableKey.contains("#") => {
        val arrayKeyIterator = variableKey.split("#").iterator
        val arrayKey = arrayKeyIterator.next()
        val arrayIndex = arrayKeyIterator.next().asInstanceOf[Int]
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

  def readMapArrayEntity[T](map:Map[String, Any], arrayKey:String, index:Int) : T = {
    map.get(arrayKey).get.asInstanceOf[Array[T]](index)
  }

  def resolveSchemas (spec: Map[String, Any], key : String, map : Map[String, Any]):Map[String, Any] = {
    var specs = spec
    var changed:Boolean = false
    specs.foreach { case (k, v) =>
      var reference = k
      var value = specs.get(reference).get
      if ( k == key)
      {
        reference = specs.get(key).get.asInstanceOf[String]
        val (found:Boolean, spec:Any) = searchKey(reference,map)
        if (found) {
          changed = true
          specs -= key
          value = spec.asInstanceOf[Map[String,Any]]
        }
      }
      var changedSpec:Map[String, Any] = null
      value match {
        case m: Map[String, Any] => {
          changedSpec = resolveSchemas(value.asInstanceOf[Map[String, Any]], key, map)
          if (changed) {
            changedSpec.foreach{ case (keys, values) =>
              specs += (keys -> values)
            }
          } else {
            specs -= reference
            specs += (reference -> changedSpec)
          }
        }
        case l: List[String] => value.asInstanceOf[List[String]]
        case b: Boolean => value.asInstanceOf[Boolean]
        case _ => value.asInstanceOf[String]
      }
    }
    (specs)
  }

  def searchKey (key : String, map : Map[String, Any]): (Boolean, Any) = {
    var found:Boolean = false
    var value:Any = null
    map.foreach { case (k, v) =>
      if (k == key) {
        value = map.get(key).get.asInstanceOf[Map[String,Any]]
        found = true
      } else {
        v match {
          case m: Map[String, Any] => {
            var (foundInSubMap, valueFound) = searchKey(key,v.asInstanceOf[Map[String, Any]])
            if (foundInSubMap) {
              found = true
              value = valueFound
              (found,value)
            }
          }
          case l: List[String] => value = v.asInstanceOf[List[String]]
          case b: Boolean => value = v.asInstanceOf[Boolean]
          case _ => value = v.asInstanceOf[String]
        }
      }
    }
    (found,value)
  }

  def searchAndResolve(key:String,parsedSpec:Map[String,Any])={
    val (found:Boolean, spec:Any) = searchKey(key,parsedSpec)
    (resolveSchemas(spec.asInstanceOf[Map[String,Any]], "$ref", parsedSpec))
  }
}
