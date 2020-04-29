package com.sumologic.terraform_generator.writer

import com.sumologic.terraform_generator.StringHelper
import com.sumologic.terraform_generator.objects.{ScalaSwaggerObject, ScalaSwaggerObjectArray, ScalaSwaggerType}

trait ResourceGeneratorHelper extends StringHelper {
  def getTerraformResourceGetters(prop: ScalaSwaggerObject): String = {
    val propName = prop.getName()
    val noCamelCaseName = removeCamelCase(propName)
    val propType = prop.getType().name
    prop match {
      case arrayProp: ScalaSwaggerObjectArray =>
        s"""${propName.capitalize}: ${propName.toLowerCase},""".stripMargin
      case singleProp: ScalaSwaggerObject =>
        if (singleProp.getName().toLowerCase == "id") {
          s"""${propName.toUpperCase}: d.Id(),""".stripMargin
        } else if (singleProp.getType().props.nonEmpty){
          s"""${propName.capitalize}: ${propName.toLowerCase},""".stripMargin
        } else {
          s"""${propName.capitalize}: d.Get(\"${noCamelCaseName.toLowerCase}\").($propType),""".stripMargin
        }
    }
  }

  def getTerraformResourceDataToObjectConverterFuncName(objClass: ScalaSwaggerType): String = {
    s"resourceTo${objClass.name}"
  }

  def getTerraformResourceDataToObjectConverter(objClass: ScalaSwaggerType, skipValidators: Boolean): String = {
    val className = objClass.name

    val getters = objClass.props.map {
      prop: ScalaSwaggerObject =>
        getTerraformResourceGetters(prop)
    }.toSet.mkString("\n    ")

    val funcName = getTerraformResourceDataToObjectConverterFuncName(objClass)

    val arrayBlock = objClass.props.filter {
      prop => prop.isInstanceOf[ScalaSwaggerObjectArray]
    }.map {
      prop =>
        if (prop.getType().props.nonEmpty) {
          s"""raw${prop.getName().capitalize} := d.Get("${removeCamelCase(prop.getName())}").([]interface{})
             |	${prop.getName().toLowerCase}List := make([]string, len(raw${prop.getName().capitalize} ))
             |	for i, v := range raw${prop.getName().capitalize}  {
             |		${prop.getName().toLowerCase}List[i] = v.(string)
             |	}""".stripMargin
        } else {
          s"""raw${prop.getName().capitalize} := d.Get("${removeCamelCase(prop.getName())}").([]interface{})
             |	${prop.getName().toLowerCase} := make([]string, len(raw${prop.getName().capitalize} ))
             |	for i, v := range raw${prop.getName().capitalize}  {
             |		${prop.getName().toLowerCase}[i] = v.(string)
             |	}""".stripMargin
        }
    }.mkString("\n")

    val propsJsonParser = objClass.props.filter {
      prop => !prop.getType.props.isEmpty
    }.map {
      prop =>
        val lowerCasePropName = prop.getType.name.toLowerCase
        val capitalizedPropName = prop.getType.name.capitalize
        if (prop.isInstanceOf[ScalaSwaggerObjectArray]) {
          s"""
             |    var ${lowerCasePropName} []${capitalizedPropName}
             |    for _, x := range ${lowerCasePropName}List {
             |        ${lowerCasePropName}Single := ${capitalizedPropName}{}
             |        ${lowerCasePropName} = append(${lowerCasePropName}, json.Unmarshal([]byte(x), &${lowerCasePropName}Single)
             |    }""".stripMargin
        } else {
          s"""s := d.Get("${removeCamelCase(prop.getName())}").(string)
             |      ${lowerCasePropName} := ${capitalizedPropName}{}
             |      json.Unmarshal([]byte(s), &${lowerCasePropName})""".stripMargin
        }
    }

    s"""func $funcName(d *schema.ResourceData) $className {
       |   $arrayBlock
       |   $propsJsonParser
       |   return $className{
       |    $getters
       |   }
       | }""".stripMargin
  }
}
