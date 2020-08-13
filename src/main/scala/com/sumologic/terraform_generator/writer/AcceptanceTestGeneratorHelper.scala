package com.sumologic.terraform_generator.writer

import java.time.{LocalDateTime, ZoneOffset}

import com.sumologic.terraform_generator.StringHelper
import com.sumologic.terraform_generator.objects.{ScalaSwaggerArrayObject, ScalaSwaggerObject, TerraformPropertyAttributes}
import nl.flotsam.xeger.Xeger

import scala.util.Random

trait AcceptanceTestGeneratorHelper extends StringHelper {
  def getTestValue(prop: ScalaSwaggerObject, isUpdate: Boolean = false): String = {
    prop.getType.name match {
      case "bool" =>
        val testBoolValue = if (prop.getDefault.isDefined) {
          prop.getDefault.get.asInstanceOf[Boolean]
        } else {
          false
        }
        testBoolValue.toString
      case "int" =>
        //TODO: Add functionality to update int
        val testIntValue = if (prop.getExample.nonEmpty) {
          prop.getExample
        }
        else if (prop.getDefault.isDefined) {
          prop.getDefault.get.toString
        } else {
          "0"
        }

        if (isUpdate && !prop.getCreateOnly) {
          (testIntValue.toLong + 1).toString
        }else {
          testIntValue
        }
      case "[]string" =>
        if (prop.getDefault.isDefined) {
          val default = prop.getDefault.get.asInstanceOf[List[String]]
          s"""[]string{"${default.head.replace("\"", "\\\"")}"}"""
        } else if (prop.getExample.nonEmpty) {
          val items = prop.getExample.replace("[", "").replace("]", "").split(",")
          s"""[]string{"${items.head.replace("\"", "\\\"")}"}"""
        } else if (prop.asInstanceOf[ScalaSwaggerArrayObject].getPattern.nonEmpty) {
          val generator = new Xeger(prop.getPattern)
          val generatedString = generator.generate()
          s"""[]string{"${generatedString.replace("\"", "\\\"")}"}"""
        } else {
          // FIXME: optimistically assuming randomStr will be less than maxLength of the prop. We should
          //  fix this and use minLength + 1 as random string length.
          val randomStr = s"${prop.getName}-${Random.alphanumeric.take(5).mkString}"
          s"""[]string{"\\"$randomStr\\""}"""
        }
      case "string" =>
        val testStringValue = if (prop.getDefault.isDefined) {
          s""""${prop.getDefault.get.toString.replace(""""""", """\"""")}""""
        } else if (prop.getExample.nonEmpty) {
          s""""${prop.getExample.replace(""""""", """\"""")}""""
        } else if (prop.getPattern.nonEmpty) {
          generateTestValueFromPattern(prop.getPattern)
        } else {
          if (prop.getFormat == "date-time") {
            s""""${LocalDateTime.now(ZoneOffset.UTC).toString.takeWhile(_ != '.')}Z""""
          } else {
            // FIXME: optimistically assuming randomStr will be less than maxLength of the prop. We should
            //  fix this and use minLength + 1 as random string length.
            val randomStr = s"${prop.getName}-${Random.alphanumeric.take(5).mkString}"
            s""""$randomStr""""
          }
        }

        val testStringValueWithAttribute = prop.getAttribute match {
          case TerraformPropertyAttributes.UNIQUE =>
            testStringValue.dropRight(1) + Random.alphanumeric.take(10).mkString("") + "\""
          case _ =>
            testStringValue
        }

        if (isUpdate && !prop.getCreateOnly) {
          val newVal = if (prop.getPattern.nonEmpty) {
            var generatedVal = ""
            do {
              generatedVal = generateTestValueFromPattern(prop.getPattern)
            } while (generatedVal.isEmpty || generatedVal == testStringValue)
            generatedVal
          } else {
            testStringValueWithAttribute.dropRight(1) + """Update""""
          }
          newVal
        } else {
          testStringValueWithAttribute
        }
      case _ =>
        throw new RuntimeException(s"Unsupported type='${prop.getType.name}', object='${prop.getName}'")
    }
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
