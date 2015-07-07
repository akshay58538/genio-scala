/*
 * Copyright 2014-2015
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * Created by prannamalai on 2/20/15.
 */
package com.paypal.genio

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.json4s.native.JsonParser

import scala.io.Source

sealed abstract class SpecType
case object SpecTypeGDD extends SpecType
case object SpecTypeSwagger extends SpecType

sealed abstract class SpecFormat
case object SpecFormatJSON extends SpecFormat
case object SpecFormatYAML extends SpecFormat

class Reader{
  def readFile (fileName:String) = {
    val content = Source.fromURL(getClass.getResource(fileName)).getLines().mkString("\n")
    fileName match {
      case file if file.endsWith("json") => (SpecFormatJSON, content)
      case file if file.endsWith("yaml") => (SpecFormatYAML, content)
      case _ => None
    }
  }

  def parseJson (json:String) = {
    implicit val formats = org.json4s.DefaultFormats
    JsonParser.parse(json).extract[Map[String, Any]]
  }

  def parseYaml (yaml:String) = {
    val parser = new ObjectMapper(new YAMLFactory())
    parser.registerModule(DefaultScalaModule)
    parser.readValue(yaml, classOf[Map[String, Any]])
  }

  def findSpecType (parsedSpec:Map[String, Any]) = {
    if(parsedSpec.get("swagger") != None)
      SpecTypeSwagger
    else if (parsedSpec.get("discoveryVersion") != None)
      SpecTypeGDD
    else
      None
  }

  def resolveRef (parsedSpec:Map[String, Any]) = {
    var spec = parsedSpec
    var schemas = spec.get("schemas").get.asInstanceOf[Map[String, Any]]
    var resolvedSchema = resolveSchemas(schemas, "$ref", schemas)
    spec -= "schemas"
    spec += ("schemas" -> resolvedSchema)
    println("final:")
    println(spec)
  }

  def resolveSchemas (spec: Map[String, Any], keys : String, schemas : Map[String, Any]):Map[String, Any] = {
    var specs = spec
    specs.foreach { case (k, v) =>
      var reference = k
      if ( k == keys)
      {
        reference = specs.get(keys).get.asInstanceOf[String]
        specs -= keys
        if (schemas.contains(reference)) {
          specs += (reference -> schemas.get(reference).get.asInstanceOf[Map[String,Any]])
        }
      }
      val value = specs.get(reference).get
      var changedSpec:Map[String, Any] = null
      value match {
          case m: Map[String, Any] => {
            changedSpec = resolveSchemas(value.asInstanceOf[Map[String, Any]], keys, schemas)
            specs -= reference
            specs += (reference -> changedSpec)
          }
          case _ => value.asInstanceOf[String]
      }
    }
    (specs)
  }

  def specType(fileName:String) = {
    val (specFormat, fileContent:String) = readFile(fileName)
    var parsedSpec:Map[String, Any] = null
    specFormat match {
      case SpecFormatJSON => parsedSpec = parseJson(fileContent)
      case SpecFormatYAML => parsedSpec = parseYaml(fileContent)
      case _ => None
    }
    resolveRef(parsedSpec)
    (findSpecType(parsedSpec), parsedSpec)
  }
}