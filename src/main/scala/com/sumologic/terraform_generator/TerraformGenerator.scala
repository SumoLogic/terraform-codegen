package com.sumologic.terraform_generator

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}

import com.sumologic.terraform_generator.objects.TerraformResource
import com.sumologic.terraform_generator.utils.{Logging, OpenApiProcessor}
import com.sumologic.terraform_generator.writer._
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.parser.core.models.{ParseOptions, SwaggerParseResult}


object TerraformGenerator
  extends StringHelper
    with Logging {

  var targetDirectory: String = "./target/"
  lazy val resourcesDirectory: String = Paths.get(targetDirectory, "resources/").toString

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
    logger.info(s"Generating provider in '$resourcesDirectory' dir")

    try {
      val parseResult = readYaml(yamlFile)
      generate(parseResult.getOpenAPI)
    } catch {
      case _: Exception => System.exit(1)
    }
  }

  def readYaml(file: String): SwaggerParseResult = {
    val parseResult = try {
      val parseOpts = new ParseOptions()
      parseOpts.setResolve(true)
      parseOpts.setResolveCombinators(true)

      new OpenAPIParser().readLocation(file, null, parseOpts)
    } catch {
      case ex: Exception =>
        logger.error(s"Failed to read $file", ex)
        throw ex
    }

    val f = new File(targetDirectory + "openapi_schema.txt")
    val bw = new BufferedWriter(new FileWriter(f))
    assert(parseResult.getOpenAPI != null)
    bw.write(parseResult.getOpenAPI.toString)
    bw.close()

    parseResult
  }

  def generate(openApi: OpenAPI): Unit = {
    try {
      val templates = OpenApiProcessor.process(openApi)
      templates.foreach {
        template =>
          logger.info(s"Resource: '${template.resourceName}' - " +
              s"${template.endpoints.head.responses.head}")
          writeFiles(template, template.resourceName)
      }

      val provider = ProviderFileGenerator(templates.map(_.resourceName))
      provider.writeToFile(Paths.get(resourcesDirectory, "provider.go").toString)
    } catch {
      case ex: Exception =>
        logger.error("Unexpected error!", ex)
        throw ex
    }
  }

  def writeFiles(terraformResource: TerraformResource, baseType: String): Unit = {
    val genSumoClass = TerraformClassFileGenerator(terraformResource)
    val terraformTypeName = removeCamelCase(baseType)

    genSumoClass.writeToFile(Paths.get(resourcesDirectory, s"sumologic_$terraformTypeName.go").toString)

    val genResource = TerraformResourceFileGenerator(terraformResource)
    genResource.writeToFile(Paths.get(resourcesDirectory, s"resource_sumologic_$terraformTypeName.go").toString)

    val genTest = AcceptanceTestFileGenerator(terraformResource, baseType)
    genTest.writeToFile(Paths.get(resourcesDirectory, s"resource_sumologic_${terraformTypeName}_test.go").toString)

    val genDocs = TerraformDocsGenerator(terraformResource, baseType)
    genDocs.writeToFile(Paths.get(resourcesDirectory, s"$terraformTypeName.html.markdown").toString)
  }
}
