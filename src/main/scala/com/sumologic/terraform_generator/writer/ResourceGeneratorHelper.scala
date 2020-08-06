package com.sumologic.terraform_generator.writer

import com.sumologic.terraform_generator.StringHelper
import com.sumologic.terraform_generator.objects._
import org.openapitools.codegen.utils.StringUtils

trait ResourceGeneratorHelper extends StringHelper {

  /**
   * Generates Terraform resource struct field and it's value. Used in converter from
   * ResourceData to Go struct.
   *
   * @param fieldScalaObj The field of resource struct to generate.
   * @return
   */
  def getResourceFieldsWithValues(fieldScalaObj: ScalaSwaggerObject): String = {
    val fieldValue = StringUtils.camelize(fieldScalaObj.getName, true)
    val fieldName = fieldScalaObj.getName.capitalize
    val schemaFieldName = StringUtils.underscore(fieldScalaObj.getName)

    fieldScalaObj match {
      case _: ScalaSwaggerArrayObject =>
        s"""$fieldName: $fieldValue,"""

      case _: ScalaSwaggerRefObject =>
        s"""$fieldName: $fieldValue,"""

      case _: ScalaSwaggerSimpleObject =>
        if (fieldScalaObj.getName.toLowerCase == "id") {
          s"""${fieldScalaObj.getName.toUpperCase}: d.Id(),"""
        } else {
          s"""$fieldName: d.Get("$schemaFieldName").(${fieldScalaObj.getGoType}),"""
        }
    }
  }


  def getResourceDataToStructFuncName(objClass: ScalaSwaggerType): String = {
    s"resourceTo${objClass.name}"
  }


  /**
   * For a given field, generates code to extract struct field from ResourceData object.
   * Used in converter from ResourceData to Go struct.
   *
   * @param fieldScalaObj The field for which to generate the extraction code.
   * @return
   */
  def extractFieldFromResourceData(fieldScalaObj: ScalaSwaggerObject): String = {
    val fieldName = StringUtils.camelize(fieldScalaObj.getName, true)
    val fieldSchemaName = StringUtils.underscore(fieldScalaObj.getName)

    fieldScalaObj match {
      case _: ScalaSwaggerArrayObject =>
        // TODO add a better way to figure out non-primitive types in case of container objects
        val fieldValue = if (fieldScalaObj.getType.props.nonEmpty) {
          // array of non-primitive type
          val funcName = getResourceDataToStructFuncName(fieldScalaObj.getType.props.head.getType)
          // TODO make sure we have a converter of funcName
          s"""$funcName(v)"""
        } else {
          // array of primitive type
          s"""v.(${fieldScalaObj.getType.name})"""
        }

        s"""
           |    ${fieldName}Data := d.Get("$fieldSchemaName").([]interface{})
           |    $fieldName := make(${fieldScalaObj.getGoType}, len(${fieldName}Data))
           |    for i, v := range ${fieldName}Data  {
           |        $fieldName[i] = $fieldValue
           |    }""".stripMargin

      case _: ScalaSwaggerRefObject =>
        val funcName = getResourceDataToStructFuncName(fieldScalaObj.getType)
        s"""$fieldName := $funcName(d.Get("$fieldSchemaName"))"""

      case _: ScalaSwaggerObject =>
        s""""""
    }
  }


  def getResourceToPropertyConverter(obj: ScalaSwaggerRefObject): String = {
    val funcName = getResourceDataToStructFuncName(obj.getType)
    val returnType = obj.getType.name

    val propName = StringUtils.camelize(obj.getName, true)
    val propArr = s"${propName}Arr"
    val propObj = s"${propName}Obj"
    val goTypeName = obj.getGoType

    val properties = obj.getType.props.map {
      case property@(_: ScalaSwaggerSimpleObject) =>
        val schemaFieldName = StringUtils.underscore(property.getName)
        val fieldName = StringUtils.camelize(property.getName)
        s"""$propName.$fieldName = $propObj["$schemaFieldName"].(${property.getGoType})"""

      case property =>
        // TODO add support for non primitive types
//        extractFieldFromResourceData(property)
        ""
    }.mkString("\n")

    val extractedProperty = s"""
      |    $propArr := data.([]interface{})
      |    $propName := $goTypeName{}
      |    if len($propArr) > 0 {
      |      $propObj := $propArr[0].(map[string]interface{})
      |      $properties
      |    }
      |""".stripMargin

    s"""
       |func $funcName(data interface{}) $returnType {
       |    $extractedProperty
       |    return $propName
       |}""".stripMargin
  }

  /**
   * Generates the converter method to extract a resource object from ResourceData.
   * i.e. resourceToObj(d *schema.ResourceData) ObjType
   *
   * @param objType ScalaSwaggerType object containing info about object to convert to.
   * @return stringified version of resourceTo<Obj> method.
   */
  def getTerraformResourceDataToObjectConverter(objType: ScalaSwaggerType): String = {
    val returnType = objType.name
    val funcName = getResourceDataToStructFuncName(objType)

    val extractedProperties = objType.props.map {
      obj => extractFieldFromResourceData(obj)
    }.mkString("\n")

    val fieldsWithValues = objType.props.map { prop =>
      getResourceFieldsWithValues(prop)
    }.toSet.mkString("\n")

    // TODO Add converters
//    val converters = objType.props.map {
//      // for each ref object or array object with ref items, create a new func resourceTo<RefObject>
//      // and use that method to extract properties in extractFieldFromResourceData method.
//      case prop: ScalaSwaggerRefObject => getResourceToPropertyConverter(prop)
//      case prop: ScalaSwaggerArrayObject => ""
//      case _ => ""
//    }.mkString("\n")

    s"""
       |func $funcName(d *schema.ResourceData) $returnType {
       |    $extractedProperties
       |    return $returnType{
       |        $fieldsWithValues
       |    }
       |}
       |""".stripMargin
  }
}
