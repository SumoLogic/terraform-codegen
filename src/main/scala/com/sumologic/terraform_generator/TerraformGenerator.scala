package com.sumologic.terraform_generator

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}

import com.sumologic.terraform_generator.objects.ScalaSwaggerTemplate
import com.sumologic.terraform_generator.utils.{Logging, OpenApiProcessor}
import com.sumologic.terraform_generator.writer._
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.parser.core.models.{ParseOptions, SwaggerParseResult}


object TerraformGenerator
  extends StringHelper
    with Logging {

  var targetDirectory: String = "./target/"
  lazy val resourcesDirectory: String = targetDirectory + "resources/"

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("==========================================================")
      println("ERROR: Insufficient args")
      println("ERROR: Usage: TerraformGenerator <yaml-file> [output-dir]")
      println("==========================================================")
      System.exit(1)
    }

    val yamlFile = args.head
    targetDirectory = if (args.length == 2) args.last else targetDirectory
    Files.createDirectories(Paths.get(resourcesDirectory))

    val swaggerParseResult = readYaml(yamlFile)
    generate(swaggerParseResult.getOpenAPI)
  }

  def readYaml(file: String): SwaggerParseResult = {
    val parseResult = try {
      val parseOpts = new ParseOptions()
      parseOpts.setResolve(true)
      parseOpts.setResolveCombinators(true)

      new OpenAPIParser().readLocation(file, null, parseOpts)
    } catch {
      case ex: RuntimeException =>
        error(s"Failed to read $file", ex)
        throw ex
    }

    val f = new File(targetDirectory + "openapi_schema.txt")
    val bw = new BufferedWriter(new FileWriter(f))
    bw.write(parseResult.getOpenAPI.toString)
    bw.close()

    parseResult
  }

  def generate(openApi: OpenAPI): Unit = {
    try {
      val templates = OpenApiProcessor.processAllClasses(openApi)
      templates.foreach {
        case (template: ScalaSwaggerTemplate, baseType: String) =>
          writeFiles(template, baseType)
      }

      val provider = ProviderFileGenerator(templates.map(_._2))
      provider.writeToFile(resourcesDirectory + "provider.go")
    } catch {
      case ex: Exception =>
        error(s"Unexpected error!", ex)
    }
  }

  def writeFiles(sumoSwaggerTemplate: ScalaSwaggerTemplate, baseType: String): Unit = {
    val genSumoClass = TerraformClassFileGenerator(sumoSwaggerTemplate)
    val terraformTypeName = removeCamelCase(baseType)

    genSumoClass.writeToFile(resourcesDirectory + s"sumologic_$terraformTypeName.go")

    val genResource = TerraformResourceFileGenerator(sumoSwaggerTemplate)
    genResource.writeToFile(resourcesDirectory + s"resource_sumologic_$terraformTypeName.go")

    val genTest = AcceptanceTestFileGenerator(sumoSwaggerTemplate, baseType)
    genTest.writeToFile(resourcesDirectory + s"resource_sumologic_${terraformTypeName}_test.go")

    val genDocs = TerraformDocsGenerator(sumoSwaggerTemplate, baseType)
    genDocs.writeToFile(resourcesDirectory + s"$terraformTypeName.html.markdown")
  }
}
