package com.sumologic.terraform_generator.writer

import com.sumologic.terraform_generator.StringHelper
import com.sumologic.terraform_generator.objects._

case class TerraformResourceFileGenerator(terraform: TerraformResource)
  extends TerraformFileGeneratorBase(terraform: TerraformResource)
    with ResourceGeneratorHelper {

  def generate(): String = {
    val specialImport = if (terraform.getResourceType.props.exists {
      prop => prop.getType.props.nonEmpty
    }) {
      """"github.com/hashicorp/terraform-plugin-sdk/helper/validation""""
    } else {
      ""
    }
    val fileHeader = s"""
                |// ----------------------------------------------------------------------------
                |//
                |//     ***     AUTO GENERATED CODE    ***    AUTO GENERATED CODE     ***
                |//
                |// ----------------------------------------------------------------------------
                |//
                |//     This file is automatically generated by Sumo Logic and manual
                |//     changes will be clobbered when the file is regenerated. Do not submit
                |//     changes to this file.
                |//
                |// ----------------------------------------------------------------------------
                |package sumologic
                |
                |import (
                |  "log"
                |  "github.com/hashicorp/terraform-plugin-sdk/helper/schema"
                |  $specialImport
                |)
                |""".stripMargin

    val mappingSchema = terraform.getResourceFuncMappings

    val ops: String = terraform.endpoints.map {
      endpoint: OpenApiEndpoint =>
        val functionGenerator = ResourceFunctionGenerator(endpoint, terraform.getResourceType)
        functionGenerator.generate
    }.mkString("\n")

    val converters = getTerraformResourceDataToObjectConverter(terraform.getResourceType)

    fileHeader + "\n" + mappingSchema + "\n" + ops + "\n" + converters
  }
}


case class ResourceFunctionGenerator(endpoint: OpenApiEndpoint, mainClass: OpenApiType)
    extends StringHelper {

  val className: String = mainClass.name
  val objName: String = lowerCaseFirstLetter(className)

  // We can have path parameter other than 'id'. Assuming for CRUD endpoints, we won't have any path
  // parameter other than 'id'.
  val hasPathParam: Boolean = endpoint.parameters.map(_.paramType).exists { param =>
    param.contains(TerraformSupportedParameterTypes.PathParameter)
  }

  val hasParams: Boolean = endpoint.parameters.map(_.paramType).exists { param =>
    param.contains(TerraformSupportedParameterTypes.QueryParameter) ||
        param.contains(TerraformSupportedParameterTypes.HeaderParameter)
  }

  val modelInResponse: Option[OpenApiResponse] = endpoint.responses.find {
    response =>
      if (response.respTypeOpt.isDefined) {
        response.respTypeOpt.get.name.toLowerCase.contains(objName.toLowerCase)
      } else {
        false
      }
  }
  val modelInParam: Option[OpenApiParameter] = endpoint.parameters.find {
    parameter =>
      parameter.param.getName.toLowerCase.contains(objName.toLowerCase)
  }
  val parameter: String = if (modelInResponse.isDefined) {
    modelInResponse.get.respTypeOpt.get.name
  } else if (modelInParam.isDefined) {
    modelInParam.get.param.getName
  } else {
    ""
  }

  val requestMap: String = if (hasParams) {
    s"""requestParams := make(map[string]string)
       |	for k, v := range d.Get("${endpoint.httpMethod.toLowerCase}_request_map").(map[string]interface{}) {
       |		requestParams[k] = v.(string)
       |	}""".stripMargin
  } else {
    ""
  }

  def generateReadFunction: String = {
    val setters = mainClass.props.filter(_.getName.toLowerCase != "id").map {
      prop: OpenApiObject =>
        val name = prop.getName
        s"""d.Set("${removeCamelCase(name)}", $objName.${name.capitalize})""".stripMargin
    }.mkString("\n    ")

    val clientCall = if (requestMap.nonEmpty) {
      s"$objName, err := c.Get$className(id, requestParams)"
    } else if (hasPathParam) {
      s"$objName, err := c.Get$className(id)"
    } else {
      s"$objName, err := c.Get$className()"
    }

  s"""
     |func resourceSumologic${className}Read(d *schema.ResourceData, meta interface{}) error {
     |	c := meta.(*Client)
     |
     |  $requestMap
     |
     |	id := d.Id()
     |	$clientCall
     |	if err != nil {
     |		return err
     |	}
     |
     |	if $objName == nil {
     |		log.Printf("[WARN] $className not found, removing from state: %v - %v", id, err)
     |		d.SetId("")
     |		return nil
     |	}
     |
     |	$setters
     |
     |	return nil
     |}""".stripMargin
  }

  def generateDeleteFunction: String = {
    val clientCall = if (requestMap.nonEmpty) {
      s"c.Delete$className(d.Id(), requestParams)"
    } else if (hasPathParam) {
      s"c.Delete$className(d.Id())"
    } else {
      s"c.Delete$className()"
    }

    s"""
       |func resourceSumologic${className}Delete(d *schema.ResourceData, meta interface{}) error {
       |  c := meta.(*Client)
       |
       |  $requestMap
       |
       |  return $clientCall
       |}""".stripMargin

  }

  def generateUpdateFunction: String = {
    val lowerCaseName = parameter.substring(0, 1).toLowerCase() + parameter.substring(1)

    val clientCall = if (requestMap.nonEmpty) {
      s"err := c.Update$className($lowerCaseName, requestParams)"
    } else {
      s"err := c.Update$className($lowerCaseName)"
    }

    s"""
       |func resourceSumologic${className}Update(d *schema.ResourceData, meta interface{}) error {
       |	c := meta.(*Client)
       |
       |  $requestMap
       |
       |	$lowerCaseName := resourceTo$className(d)
       |	$clientCall
       |	if err != nil {
       |		return err
       |	}
       |
       |	return resourceSumologic${className}Read(d, meta)
       |}""".stripMargin
  }

  def generateCreateFunction: String = {
    val lowerCaseName = parameter.substring(0, 1).toLowerCase() + parameter.substring(1)

    val clientCall = if (requestMap.nonEmpty) {
      s"id, err := c.Create$className($lowerCaseName, requestParams)"
    } else {
      s"id, err := c.Create$className($lowerCaseName)"
    }

    s"""
       |func resourceSumologic${className}Create(d *schema.ResourceData, meta interface{}) error {
       |	c := meta.(*Client)
       |
       |  $requestMap
       |
       |	if d.Id() == "" {
       |		$lowerCaseName := resourceTo$className(d)
       |		$clientCall
       |		if err != nil {
       |			return err
       |		}
       |
       |		d.SetId(id)
       |	}
       |
       |	return resourceSumologic${className}Read(d, meta)
       |}""".stripMargin

  }

  def generateExistsFunction: String = {
    s"""
       |func resourceSumologic${className}Exists(d *schema.ResourceData, meta interface{}) error {
       |	c := meta.(*Client)
       |
       |	_, err := c.Get$className(d.Id())
       |	if err != nil {
       |		return err
       |	}
       |
       |	return nil
       |}""".stripMargin
  }

  def generate: String = {
    endpoint.endpointType match {
      case TerraformPathExtensions.Create => generateCreateFunction
      case TerraformPathExtensions.Read => generateReadFunction
      case TerraformPathExtensions.Update => generateUpdateFunction
      case TerraformPathExtensions.Delete => generateDeleteFunction
    }
  }
}

