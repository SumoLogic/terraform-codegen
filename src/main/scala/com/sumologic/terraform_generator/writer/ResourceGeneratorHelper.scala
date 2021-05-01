package com.sumologic.terraform_generator.writer

import com.sumologic.terraform_generator.objects.{OpenApiObject, _}
import org.openapitools.codegen.utils.StringUtils

trait ResourceGeneratorHelper {

  /**
   * Generates Terraform resource struct field and it's value. Used in converter from
   * ResourceData to Go struct.
   *
   * @param fieldScalaObj The field of resource struct to generate.
   * @return Go code to set field 'fieldScalaObj' to its value.
   */
  def getResourceFieldsWithValues(fieldScalaObj: OpenApiObject): String = {
    val fieldValue = StringUtils.camelize(fieldScalaObj.getName, true)
    val fieldName = fieldScalaObj.getName.capitalize
    val schemaFieldName = StringUtils.underscore(fieldScalaObj.getName)

    fieldScalaObj match {
      case _: OpenApiArrayObject =>
        s"""$fieldName: $fieldValue,"""

      case _: OpenApiRefObject =>
        s"""$fieldName: $fieldValue,"""

      case _: OpenApiSimpleObject =>
        if (fieldScalaObj.getName.toLowerCase == "id") {
          s"""${fieldScalaObj.getName.toUpperCase}: d.Id(),"""
        } else {
          s"""$fieldName: d.Get("$schemaFieldName").(${fieldScalaObj.getGoType}),"""
        }
    }
  }


  def getResourceDataToStructFuncName(objClass: OpenApiType): String = {
    s"resourceTo${objClass.name}"
  }


  /**
   * Helper method for extractFieldFromResourceData method.
   * For a given field of array type, generates code to extract the given field from ResourceData object
   * represented by 'goObjName' argument.
   *
   * @param arrayObj The field for which to generate the extraction code.
   * @param goObjName The name of the golang Terraform object that represents the given field.
   * @param fieldName The name of the golang var in which the field will be extracted.
   *
   * @return Go code to extract a field from ResourceData object.
   */
  def extractArrayItems(arrayObj: OpenApiArrayObject, goObjName: String, fieldName: String): String = {
    val items = arrayObj.items
    items match {
      case arrayItem: OpenApiArrayObject =>
        // Only nested arrays of 1 level are supported currently.
        // Need to generate unique names for itemSliceName and newGoObjName to support nested arrays of
        // any depth.
        val itemSliceName = "itemSlice"
        val newGoObjName = "item"
        val arrayItems = extractArrayItems(arrayItem, newGoObjName, itemSliceName)

        s"""|${goObjName}Slice := $goObjName.([]interface{})
            |var $itemSliceName ${arrayItem.getGoType}
            |for _, $newGoObjName := range ${goObjName}Slice {
            |    $arrayItems
            |}
            |$fieldName = append($fieldName, $itemSliceName)""".stripMargin

      case _: OpenApiRefObject =>
        val funcName = getResourceDataToStructFuncName(items.getType)
        s"""|$fieldName = append($fieldName, $funcName([]interface{}{$goObjName}))""".stripMargin

      case _:OpenApiSimpleObject =>
        s"""$fieldName = append($fieldName, $goObjName.(${items.getGoType})) """
    }
  }


  /**
   * For a given field, generates code to extract struct field from ResourceData object.
   * Used in converter from ResourceData to Go struct.
   *
   * @param fieldScalaObj The field for which to generate the extraction code.
   * @return Go code to extract a field from ResourceData object.
   */
  def extractFieldFromResourceData(fieldScalaObj: OpenApiObject): String = {
    val fieldName = StringUtils.camelize(fieldScalaObj.getName, true)
    val fieldSchemaName = StringUtils.underscore(fieldScalaObj.getName)

    fieldScalaObj match {
      case arrayObj: OpenApiArrayObject =>
        s"""
           |${fieldName}Data := d.Get("$fieldSchemaName").([]interface{})
           |var $fieldName ${fieldScalaObj.getGoType}
           |for _, data := range ${fieldName}Data  {
           |    ${extractArrayItems(arrayObj, "data", s"$fieldName")}
           |}
           |""".stripMargin

      case _: OpenApiRefObject =>
        val funcName = getResourceDataToStructFuncName(fieldScalaObj.getType)
        s"""$fieldName := $funcName(d.Get("$fieldSchemaName"))"""

      case _: OpenApiObject =>
        s""""""
    }
  }


  /**
   * For a given OpenApiObject object, finds out all non-primitive objects in it and
   * all of it's descendants by recursively traversing the object.
   *
   * @param obj Top level OpenApiObject to start iteration from.
   * @return All non-primitive objects in 'obj' arg and descendants.
   */
  def getAllRefObjects(obj: OpenApiObject): List[OpenApiRefObject] = {
    obj match {
      case refObject: OpenApiRefObject =>
        val converters = obj.getType.props.flatMap { prop =>
          getAllRefObjects(prop)
        }
        List(refObject) ++ converters

      case arrObject: OpenApiArrayObject if arrObject.getType.props.nonEmpty =>
        // FIXME: Create OpenApiRefObject from prop for item contained with in array.
        //  This is a hacky solution. OpenApiArrayObject should have a data member of
        //  OpenApiObject type to capture item object.
        val itemObject = OpenApiRefObject(
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
        List.empty[OpenApiRefObject]
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
   * @return Go code to set field to its value.
   */
  def extractFieldFromMap(fieldScalaObj: OpenApiObject, goObjectName: String, goMapObject: String): String = {
    val fieldName = StringUtils.camelize(fieldScalaObj.getName)
    val fieldSchemaName = StringUtils.underscore(fieldScalaObj.getName)

    fieldScalaObj match {
      case _: OpenApiArrayObject =>
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

      case _: OpenApiRefObject =>
        val funcName = getResourceDataToStructFuncName(fieldScalaObj.getType)
        s"""$goObjectName.$fieldName = $funcName($goMapObject["$fieldSchemaName"])"""

      case _: OpenApiObject =>
        s"""$goObjectName.$fieldName = $goMapObject["$fieldSchemaName"].(${fieldScalaObj.getGoType})"""
    }
  }


  /**
   * Generates a converter method that converts a ResourceData map to non-primitive (ref) object
   *
   * A non-primitive object is represented as a list of one item in Terraform schema. Extracts
   * properties from ResourceData and creates an object of {{goObjName.objType.name}} type.
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
   * @param obj OpenApiRefObject to extract from ResourceData object.
   * @return Go code of converter method.
   */
  def getRefObjectConverter(obj: OpenApiRefObject): String = {
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
   * Example converter method generated by this method:
   *
   * func resourceToLookupTable(d *schema.ResourceData) LookupTable {
   *   fieldsData := d.Get("fields").([]interface{})
   *   var fields []LookupTableField
   *   for _, data := range fieldsData {
   *     fields = append(fields, resourceToLookupTableField([]interface{}{data}))
   *   }
   *
   *   primaryKeysData := d.Get("primary_keys").([]interface{})
   *   var primaryKeys []string
   *   for _, data := range primaryKeysData {
   *     primaryKeys = append(primaryKeys, data.(string))
   *   }
   *
   *   return LookupTable{
   *     Name:            d.Get("name").(string),
   *     ID:              d.Id(),
   *     Fields:          fields,
   *     Description:     d.Get("description").(string),
   *     Ttl:             d.Get("ttl").(int),
   *     SizeLimitAction: d.Get("size_limit_action").(string),
   *     PrimaryKeys:     primaryKeys,
   *     ParentFolderId:  d.Get("parent_folder_id").(string),
   *   }
   * }
   *
   *
   * @param objType OpenApiType object containing info about object to convert to.
   * @return Go code of resourceTo<Obj> method.
   */
  def getTerraformResourceDataToObjectConverter(objType: OpenApiType): String = {
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
