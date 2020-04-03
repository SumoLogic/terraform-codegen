package com.sumologic.terraform_generator.writer

import com.sumologic.terraform_generator.objects.{SumoSwaggerTemplate, SumoSwaggerType, SumoTerraformEntity}

case class SumoDocsGenerator(terraform: SumoSwaggerTemplate, mainClass: String)
  extends SumoTerraformFileGenerator(terraform: SumoSwaggerTemplate) {
  val functionGenerator = SwaggerDocsFunctionGenerator(terraform, terraform.getAllTypesUsed().filter(_.name.toLowerCase.contains(mainClass.toLowerCase)).head)
  def generate(): String = {
    functionGenerator.generateLayout() + functionGenerator.generateHeader() + functionGenerator.generateExampleUsage() + functionGenerator.generateArgReferences() + functionGenerator.generateFooter()
  }
}

case class SwaggerDocsFunctionGenerator(sumoSwaggerTemplate: SumoSwaggerTemplate, mainClass: SumoSwaggerType) extends SumoTerraformEntity {
  val className = mainClass.name
  val objName = className.substring(0, 1).toLowerCase() + className.substring(1)
  val resourceProps = sumoSwaggerTemplate.getAllTypesUsed().head

  def generateLayout(): String = {
    s"""---
       |layout: "sumologic"
       |page_title: "SumoLogic: sumologic_${removeCamelCase(objName)}"
       |description: |-
       |  Provides a Sumologic ${addSpace(className)}
       |---
       |""".stripMargin
  }

  def generateHeader(): String = {
    s"""
       |# sumologic_$objName
       |Provider to manage Sumologic ${addSpace(className)}s
       |
       |""".stripMargin
  }

  def generateExampleUsage(): String = {
    val terraformArgs = resourceProps.props.filter(_.getName.toLowerCase != "id").map {
      prop =>
        s"""${removeCamelCase(prop.getName())} = "${prop.getExample()}""""
    }.mkString("\n    ")

    s"""## Example Usage
       |```hcl
       |resource "sumologic_${removeCamelCase(objName)}" "example_${removeCamelCase(objName)}" {
       |    $terraformArgs
       |}
       |```
       |""".stripMargin
  }

  def generateArgReferences(): String = {
    val terraformArgs = resourceProps.props.filter(_.getName.toLowerCase != "id").map {
      prop =>
        if (prop.getRequired()) {
          s"""- `${removeCamelCase(prop.getName())}` - (Required) ${prop.getDescription()}"""
        } else {
          s"""- `${removeCamelCase(prop.getName())}` - (Optional) ${prop.getDescription()}"""
        }
    }.mkString("\n")

    val exportedArgs = if (resourceProps.props.map(_.getName.toLowerCase).contains("id")) {
      s"""The following attributes are exported:
         |
         |- `id` - The internal ID of the ${removeCamelCase(objName)}
         |""".stripMargin
    } else {
      ""
    }

    s"""## Argument reference
       |
       |The following arguments are supported:
       |
       |$terraformArgs
       |
       |$exportedArgs
       |
       |""".stripMargin
  }

  def generateFooter(): String = {
    s"""
       |[Back to Index][0]
       |
       |[0]: ../README.md
       |""".stripMargin
  }
}