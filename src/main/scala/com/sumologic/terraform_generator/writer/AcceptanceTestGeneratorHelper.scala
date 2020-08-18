package com.sumologic.terraform_generator.writer

import java.time.{LocalDateTime, ZoneOffset}

import com.sumologic.terraform_generator.StringHelper
import com.sumologic.terraform_generator.objects.{ScalaSwaggerArrayObject, ScalaSwaggerObject, ScalaSwaggerSimpleObject, TerraformPropertyAttributes}
import nl.flotsam.xeger.Xeger

import scala.util.Random

trait AcceptanceTestGeneratorHelper extends StringHelper {

  def getTestValue(prop: ScalaSwaggerObject, isUpdate: Boolean = false): String = {
    prop match {
      case prop if prop.isInstanceOf[ScalaSwaggerSimpleObject] =>
        prop.getType.name match {
          case "bool" =>
            val testBoolValue = generateBoolValue(prop)
            testBoolValue.toString

          case "int" =>
            //TODO: Add functionality to update int
            val testLongValue = if (isUpdate && !prop.getCreateOnly) {
              generateLongValue(prop) + 1
            } else {
              generateLongValue(prop)
            }
            testLongValue.toString

          case "string" =>
            val testStringValue = generateStringValue(prop)

            if (isUpdate && !prop.getCreateOnly) {
              val newVal = if (prop.getPattern.nonEmpty) {
                var generatedVal = ""
                do {
                  generatedVal = generateTestValueFromPattern(prop.getPattern)
                } while (generatedVal.isEmpty || generatedVal == testStringValue)
                generatedVal
              } else {
                testStringValue.dropRight(1) + """Update""""
              }
              newVal
            } else {
              testStringValue
            }

          case _ =>
            throw new RuntimeException(s"Unsupported type='${prop.getType.name}', property='${prop.getName}'")
        }

      case prop if prop.isInstanceOf[ScalaSwaggerArrayObject] =>
        prop.getType.name match {
          case "string" =>
            val item = if (prop.getDefault.isDefined) {
                val default = prop.getDefault.get.asInstanceOf[List[String]]
                default.head.replace("\"", "\\\"")
              } else if (prop.getExample.nonEmpty) {
                val items = prop.getExample.replace("[", "").replace("]", "").split(",")
                items.head.replace("\"", "\\\"")
              } else if (prop.asInstanceOf[ScalaSwaggerArrayObject].getPattern.nonEmpty) {
                val generator = new Xeger(prop.getPattern)
                val generatedStr = generator.generate()
                generatedStr.replace("\"", "\\\"")
              } else {
                // FIXME: optimistically assuming randomStr will be less than maxLength of the prop.
                //  We should fix this and use minLength + 1 as random string length.
                val randomStr = s"${prop.getName}-${Random.alphanumeric.take(5).mkString}"
                s"""\\"$randomStr\\""""
              }
            s"""[]string{"$item"}"""

          case "bool" =>
            throw new RuntimeException(s"Unsupported type='${prop.getType.name}', property='${prop.getName}'")
          case "int" =>
            throw new RuntimeException(s"Unsupported type='${prop.getType.name}', property='${prop.getName}'")
          case _ =>
            // TODO Add support for non primitive types.
            throw new RuntimeException(s"Unsupported type='${prop.getType.name}', property='${prop.getName}'")
        }
    }
  }

  def generateBoolValue(prop: ScalaSwaggerObject): Boolean = {
    if (prop.getDefault.isDefined) {
      prop.getDefault.get.asInstanceOf[Boolean]
    } else {
      false
    }
  }

  def generateLongValue(prop: ScalaSwaggerObject): Long = {
    if (prop.getExample.nonEmpty) {
      prop.getExample.toLong
    } else if (prop.getDefault.isDefined) {
      prop.getDefault.get.toString.toLong
    } else {
      // FIXME: 0 might not be a valid value in some cases. Have to rely on default or example.
      0L
    }
  }

  def generateStringValue(prop: ScalaSwaggerObject): String = {
    val stringValue = if (prop.getDefault.isDefined) {
      s""""${prop.getDefault.get.toString.replace(""""""", """\"""")}""""
    } else if (prop.getExample.nonEmpty) {
      s""""${prop.getExample.replace(""""""", """\"""")}""""
    } else if (prop.getPattern.nonEmpty) {
      generateTestValueFromPattern(prop.getPattern)
    } else {
      if (prop.getFormat == "date-time") {
        s""""${LocalDateTime.now(ZoneOffset.UTC).toString.takeWhile(_ != '.')}Z""""
      } else {
        // FIXME: optimistically assuming randomStr will be less than maxLength of the prop.
        //  We should fix this and use minLength + 1 as random string length.
        val randomStr = s"${prop.getName}-${Random.alphanumeric.take(5).mkString}"
        s""""$randomStr""""
      }
    }

    val testStringValue = prop.getAttribute match {
      case TerraformPropertyAttributes.UNIQUE =>
        stringValue.dropRight(1) + Random.alphanumeric.take(10).mkString("") + "\""
      case _ =>
        stringValue
    }

    testStringValue
  }

  private def generateTestValueFromPattern(pattern: String): String = {
    val generator = new Xeger(pattern)
    // NOTE: Xeger doesn't support all valid regular expressions. A regex like "^(Opt1|opt2)$"
    // would end up generating value like "^Opt1$" which doesn't work. Doing a fix locally till
    // we fix all regular expressions in the yaml files.
    // https://code.google.com/archive/p/xeger/wikis/XegerLimitations.wiki
    val testValue = generator.generate().replace("^", "").replace("$", "")
    s""""${testValue.replace(""""""", """\"""")}""""
  }
}
