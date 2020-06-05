package com.sumologic.terraform_generator.writer

import java.time.LocalDateTime

import com.sumologic.terraform_generator.StringHelper
import com.sumologic.terraform_generator.objects.{ScalaSwaggerObject, ScalaSwaggerObjectArray}
import nl.flotsam.xeger.Xeger

import scala.util.Random

trait AcceptanceTestGeneratorHelper extends StringHelper {
  def getTestValue(prop: ScalaSwaggerObject, isUpdate: Boolean = false, isInUpdateRequest: Boolean = false): String = {
    prop.getType().name match {
      case "bool" =>
        if (prop.getDefault().isDefined) {
          prop.getDefault().get.toString
        } else {
          "false"
        }
      case "int64" | "int32" | "int" =>
        //TODO: Add functionality to update ints
        if (prop.getExample().nonEmpty) {
          prop.getExample()
        }
        else if (prop.getDefault().isDefined) {
          prop.getDefault().get.toString
        } else {
          "0"
        }
      case "[]string" =>
        if (prop.getDefault().isDefined) {
          val default = prop.getDefault().get.asInstanceOf[List[String]]
          s"""[]string{"${default.head.replace("\"", "\\\"")}"}"""
        } else if (prop.getExample().nonEmpty) {
          val items = prop.getExample().replace("[", "").replace("]", "").split(",")
          s"""[]string{"${items.head.replace("\"", "\\\"")}"}"""
        } else if (prop.asInstanceOf[ScalaSwaggerObjectArray].getPattern().nonEmpty) {
          val generator = new Xeger(prop.getPattern())
          val generatedString = generator.generate()
          s"""[]string{"${generatedString.replace("\"", "\\\"")}"}"""
        } else {
          val r = new Random
          val sb = new StringBuilder
          for (i <- 1 to 10) {
            sb.append(r.nextPrintableChar)
          }
          s"""[]string{"${sb.toString().replace("\"", "\\\"")}"}"""
        }
      case "string" =>
        //NOTE: Making an assumption that there will always be at least one string field
        val update = if (isUpdate && prop.getName.toLowerCase.contains("name")) {"Update"} else {""}
        if (prop.getDefault().isDefined) {
          s""""${prop.getDefault().get.toString.replace(""""""", """\"""")}$update""""
        } else if (prop.getExample().nonEmpty) {
          s""""${prop.getExample().toString.replace(""""""", """\"""")}$update""""
        } else if (prop.getPattern().nonEmpty) {
          val generator = new Xeger(prop.getPattern())
          generator.generate()
          s""""${generator.generate().replace(""""""", """\"""")}""""
        } else {
          if (prop.getFormat() == "date-time") {
            s""""${LocalDateTime.now().toString.dropRight(1)}Z""""
          } else {
            val r = new Random
            val sb = new StringBuilder
            for (i <- 1 to 10) {
              sb.append(r.nextPrintableChar)
            }
            s"""${sb.toString.replace(""""""", """\"""")}"""
          }
        }
      case _ =>
        throw new RuntimeException("Trying to generate test values for an unsupported type.")
    }
  }
}
