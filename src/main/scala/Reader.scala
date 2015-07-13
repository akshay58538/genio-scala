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
    Source.fromURL(getClass.getResource(fileName)).getLines().mkString("\n")
  }

  def readWebUrl (WbUrl:String) = {
    Source.fromURL(WbUrl).mkString
  }

  def specFormat (fileName:String):Option[SpecFormat] = {
    fileName match {
      case file if file.endsWith("json") => Option(SpecFormatJSON)
      case file if file.endsWith("yaml") => Option(SpecFormatYAML)
      case _ => None
    }
  }

  def parser (resourceName:String, content:String) = {
    var parsedSpec:Map[String, Any] = null
    val format = specFormat(resourceName).get
    format match {
      case SpecFormatJSON => parsedSpec = parseJson(content)
      case SpecFormatYAML => parsedSpec = parseYaml(content)
      case _ => None
    }
    parsedSpec
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

  def readSpec(fileName:String) = {
    val fileContent:String = readFile(fileName)
    val parsedSpec = parser(fileName, fileContent)
    (findSpecType(parsedSpec), parsedSpec)
  }
}