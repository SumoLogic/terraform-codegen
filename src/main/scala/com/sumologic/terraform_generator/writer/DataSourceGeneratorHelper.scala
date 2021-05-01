package com.sumologic.terraform_generator.writer

import com.sumologic.terraform_generator.StringHelper
import com.sumologic.terraform_generator.objects.{OpenApiObject, OpenApiType}

trait DataSourceGeneratorHelper extends StringHelper {
  def getTerraformResourceSetters(propName: String, objName: String): String = {
    val noCamelCaseName = removeCamelCase(propName)
    s"""resourceData.Set("${noCamelCaseName.toLowerCase}", $objName.${propName.capitalize})""".stripMargin
  }

  def getTerraformDataSourceValueSetCheck(varName: String,
                                          varTypeOpt: Option[String] = None,
                                          singleReturn: Boolean = false): String = {
    val singleReturnTxt = if (singleReturn) {
      ""
    } else {
      "nil, "
    }
    val typeInfo = varTypeOpt match {
      case Some(varType) => s" to construct $varType"
      case None => ""
    }
    val noCamelCaseName = removeCamelCase(varName)
    if (varName.toLowerCase.contains("id")) {
      ""
    } else {
      s"""${varName}Exists, ok := resourceData.GetOkExists("$noCamelCaseName");
         |if !ok {
         |    return ${singleReturnTxt}fmt.Errorf("SumologicTerraformError: %q is required$typeInfo.", ${varName}Exists)
         |  }\n""".stripMargin
    }
  }

  def getTerraformObjectToResourceDataConverterFuncCall(objClass: OpenApiType): String = {
    val objName = lowerCaseFirstLetter(objClass.name)
    s"${objName}ToResourceData(resourceData, $objName)"
  }

  def getTerraformObjectToResourceDataConverter(objClass: OpenApiType): String = {
    val className = objClass.name
    val objName = lowerCaseFirstLetter(objClass.name)

    val setters = objClass.props.map {
      prop: OpenApiObject =>
        getTerraformResourceSetters(prop.getName, objName)
    }.mkString("\n")

    s"""func ${objName}ToResourceData(resourceData *schema.ResourceData, $objName *$className) {
       |   $setters
       | }""".stripMargin

  }
}
