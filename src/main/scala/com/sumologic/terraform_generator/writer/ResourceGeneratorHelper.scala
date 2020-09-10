package com.sumologic.terraform_generator.writer

import com.sumologic.terraform_generator.objects.{ScalaSwaggerObject, _}
import org.openapitools.codegen.utils.StringUtils

trait ResourceGeneratorHelper {

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


  def extractArrayItems(arrayObj: ScalaSwaggerArrayObject, obj: String, fieldName: String): String = {
    val items = arrayObj.items
    items match {
      case arrayItem: ScalaSwaggerArrayObject =>
        val tmpSliceName = "tmpSlice"
        val newObjName = "d"
        val arrayItems = extractArrayItems(arrayItem, newObjName, tmpSliceName)

        s"""|${obj}Slice := $obj.([]interface{})
            |var $tmpSliceName ${arrayItem.getGoType}
            |for _, $newObjName := range ${obj}Slice {
            |    $arrayItems
            |}
            |$fieldName = append($fieldName, $tmpSliceName)""".stripMargin

      case _: ScalaSwaggerRefObject =>
        val itemType = items.getType
        assert(itemType.toString == arrayObj.getType.toString,
          s"itemType=$itemType, fieldScalaObj=${arrayObj.getType}")
        // TODO replace fieldScalaObj.getType with itemType
        val funcName = getResourceDataToStructFuncName(arrayObj.getType)
        s"""|$fieldName = append($fieldName, $funcName([]interface{}{$obj}))""".stripMargin

      case _:ScalaSwaggerSimpleObject =>
        // s"""$fieldName[i] = v.(${arrayObj.getType.name})"""
        s"""$fieldName = append($fieldName, $obj.(${arrayObj.getType.name})) """
    }

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
      case arrayObj: ScalaSwaggerArrayObject =>
        s"""
           |${fieldName}Data := d.Get("$fieldSchemaName").([]interface{})
           |var $fieldName ${fieldScalaObj.getGoType}
           |for _, data := range ${fieldName}Data  {
           |    ${extractArrayItems(arrayObj, "data", s"$fieldName")}
           |}
           |""".stripMargin

      case _: ScalaSwaggerRefObject =>
        val funcName = getResourceDataToStructFuncName(fieldScalaObj.getType)
        s"""$fieldName := $funcName(d.Get("$fieldSchemaName"))"""

      case _: ScalaSwaggerObject =>
        s""""""
    }
  }


  /**
   * For a given ScalaSwaggerObject object, finds out all non-primitive objects in it and
   * all of it's descendants by recursively traversing the object.
   *
   * @param obj Top level ScalaSwaggerObject to start iteration from.
   * @return All non-primitive objects in obj arg and descendants.
   */
  def getAllRefObjects(obj: ScalaSwaggerObject): List[ScalaSwaggerRefObject] = {
    obj match {
      case refObject: ScalaSwaggerRefObject =>
        val converters = obj.getType.props.flatMap { prop =>
          getAllRefObjects(prop)
        }
        List(refObject) ++ converters

      case arrObject: ScalaSwaggerArrayObject if arrObject.getType.props.nonEmpty =>
        // FIXME: Create ScalaSwaggerRefObject from prop for item contained with in array.
        //  This is a hacky solution. ScalaSwaggerArrayObject should have a data member of
        //  ScalaSwaggerObject type to capture item object.
        val itemObject = ScalaSwaggerRefObject(
          StringUtils.camelize(arrObject.getType.name, true),
          arrObject.getType,
          arrObject.getRequired,
          arrObject.getDefault,
          arrObject.getDescription,
          arrObject.getExample,
          arrObject.getPattern,
          arrObject.getFormat,
          arrObject.getAttribute,
          arrObject.getCreateOnly
        )
        getAllRefObjects(itemObject)

      case _ =>
        List.empty[ScalaSwaggerRefObject]
    }
  }


  /**
   * Helper method for {{getRefObjectConverter}}.
   * Extracts a field from ResourceData map ({{goMapObject}} and sets it in the
   * struct {{goObjectName}}.
   *
   * @param fieldScalaObj Field to extract.
   * @param goObjectName Name of Go struct which contains the field.
   * @param goMapObject Name of ResourceData map that contains the value of the field.
   * @return String version of field set to its value.
   */
  def extractFieldFromMap(fieldScalaObj: ScalaSwaggerObject, goObjectName: String, goMapObject: String): String = {
    val fieldName = StringUtils.camelize(fieldScalaObj.getName)
    val fieldSchemaName = StringUtils.underscore(fieldScalaObj.getName)

    fieldScalaObj match {
      case _: ScalaSwaggerArrayObject =>
        // TODO add a better way to figure out non-primitive types in case of container objects
        val fieldValue = if (fieldScalaObj.getType.props.nonEmpty) {
          // array of non-primitive type
          val funcName = getResourceDataToStructFuncName(fieldScalaObj.getType)
          s"""$funcName(v)"""
        } else {
          // array of primitive type
          s"""v.(${fieldScalaObj.getType.name})"""
        }

        val fieldVar = fieldName.head.toLower + fieldName.tail
        s"""
           |${fieldVar}Data := $goMapObject["$fieldSchemaName"].([]interface{})
           |$fieldVar := make(${fieldScalaObj.getGoType}, len(${fieldVar}Data))
           |for i, v := range ${fieldVar}Data  {
           |    $fieldVar[i] = $fieldValue
           |}
           |$goObjectName.$fieldName = $fieldVar
           |""".stripMargin

      case _: ScalaSwaggerRefObject =>
        val funcName = getResourceDataToStructFuncName(fieldScalaObj.getType)
        s"""$goObjectName.$fieldName = $funcName($goMapObject["$fieldSchemaName"])"""

      case _: ScalaSwaggerObject =>
        s"""$goObjectName.$fieldName = $goMapObject["$fieldSchemaName"].(${fieldScalaObj.getGoType})"""
    }
  }


  /**
   * Generates a converter method that converts a ResourceData map to non-primitive (ref) object
   *
   * A non-primitive object is represented as a list of one item in Terraform schema. Extracts
   * properties from ResourceData and creates an object of {{obj.objType.name}} type.
   *
   * Example converter method generated by this method:
   * func resourceToLookupTableField(data interface{}) LookupTableField {
   *    lookupTableFieldSlice := data.([]interface{})
   *    lookupTableField := LookupTableField{}
   *    if len(lookupTableFieldSlice) > 0 {
   *        lookupTableFieldObj := lookupTableFieldSlice[0].(map[string]interface{})
   *        lookupTableField.FieldName = lookupTableFieldObj["field_name"].(string)
   *        lookupTableField.FieldType = lookupTableFieldObj["field_type"].(string)
   *    }
   *    return lookupTableField
   * }
   *
   * @param obj ScalaSwaggerRefObject to extract from ResourceData object.
   * @return String representation of converter method.
   */
  def getRefObjectConverter(obj: ScalaSwaggerRefObject): String = {
    val funcName = getResourceDataToStructFuncName(obj.getType)
    val returnType = obj.getType.name

    val propName = StringUtils.camelize(obj.getName, true)
    val propArr = s"${propName}Slice"
    val propObj = s"${propName}Obj"
    val goTypeName = obj.getGoType

    val properties = obj.getType.props.map { fieldObj =>
      extractFieldFromMap(fieldObj, propName, propObj)
    }.mkString("\n")

    val extractedProperty = s"""
      |$propArr := data.([]interface{})
      |$propName := $goTypeName{}
      |if len($propArr) > 0 {
      |  $propObj := $propArr[0].(map[string]interface{})
      |  $properties
      |}
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
   * @return String representation of resourceTo<Obj> method.
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

    // Find out all non-primitive types and generate converter methods i.e. resourceTo<FieldObject>
    // method to extract properties from ResourceData and create an object of FieldObject type.
    val nonPrimitiveObjects = objType.props.flatMap {
      getAllRefObjects
    }.distinct
    val converters = nonPrimitiveObjects.map { obj =>
      getRefObjectConverter(obj)
    }.mkString("\n")

    s"""
       |func $funcName(d *schema.ResourceData) $returnType {
       |    $extractedProperties
       |    return $returnType{
       |        $fieldsWithValues
       |    }
       |}
       |$converters
       |""".stripMargin
  }
}
