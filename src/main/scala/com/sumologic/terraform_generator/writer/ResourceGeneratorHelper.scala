package com.sumologic.terraform_generator.writer

import com.sumologic.terraform_generator.StringHelper
import com.sumologic.terraform_generator.objects.{SumoSwaggerObject, SumoSwaggerObjectArray, SumoSwaggerType}

trait ResourceGeneratorHelper extends StringHelper {
  def getTerraformResourceGetters(prop: SumoSwaggerObject): String = {
    val propName = prop.getName()
    val noCamelCaseName = removeCamelCase(propName)
    val propType = prop.getType().name
    prop match {
      case arrayProp: SumoSwaggerObjectArray =>
        s"""${propName.capitalize}: ${propName.toLowerCase},""".stripMargin
      case singleProp: SumoSwaggerObject =>
        if (singleProp.getName().toLowerCase == "id") {
          s"""${propName.toUpperCase}: d.Id(),""".stripMargin
        } else {
          s"""${propName.capitalize}: d.Get(\"${noCamelCaseName.toLowerCase}\").($propType),""".stripMargin
        }
    }
  }

  def getTerraformResourceDataToObjectConverterFuncName(objClass: SumoSwaggerType): String = {
    s"resourceTo${objClass.name}"
  }

  def getTerraformResourceDataToObjectConverter(objClass: SumoSwaggerType, skipValidators: Boolean): String = {
    val className = objClass.name

    val getters = objClass.props.map {
      prop: SumoSwaggerObject =>
        getTerraformResourceGetters(prop)
    }.toSet.mkString("\n    ")

    val funcName = getTerraformResourceDataToObjectConverterFuncName(objClass)

    val arrayBlock = objClass.props.filter {
      prop => prop.isInstanceOf[SumoSwaggerObjectArray]
    }.map {
      prop => s"""raw${prop.getName().capitalize} := d.Get("${removeCamelCase(prop.getName())}").([]interface{})
                 |	${prop.getName().toLowerCase} := make([]string, len(raw${prop.getName().capitalize} ))
                 |	for i, v := range raw${prop.getName().capitalize}  {
                 |		${prop.getName().toLowerCase}[i] = v.(string)
                 |	}""".stripMargin
    }.mkString("\n")
    s"""func $funcName(d *schema.ResourceData) $className {
       |   $arrayBlock
       |   return $className{
       |    $getters
       |   }
       | }""".stripMargin
  }
}
