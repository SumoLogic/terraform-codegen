package com.sumologic.terraform_generator

import java.io.{BufferedWriter, File, FileWriter}

import com.sumologic.terraform_generator.objects.ScalaSwaggerTemplate
import com.sumologic.terraform_generator.utils.OpenApiProcessor
import com.sumologic.terraform_generator.writer.{TerraformDocsGenerator, SumoTerraformClassFileGenerator, TerraformResourceFileGenerator, AcceptanceTestFileGenerator}
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions


object TerraformGenerator extends StringHelper {

  val targetDirectory = "./target/"
  val resourcesDirectory: String = targetDirectory + "resources/"

  def main(args: Array[String]): Unit = {
    val inputFile = args(0)
    val types = if (args.size > 1) {
      args.drop(1).toList
    } else {
      List.empty[String]
    }

    val parseOpts = new ParseOptions()
    parseOpts.setResolve(true)
    parseOpts.setResolveCombinators(true)

    ensureDirectories()

    val swagger = if (inputFile.endsWith(".yml")) {
      new OpenAPIParser().readLocation(inputFile, null, parseOpts)
    } else {
      new OpenAPIParser().readContents(inputFile, null, parseOpts)
    }
    val f = new File(targetDirectory + "openapi_schema.txt")
    val bw = new BufferedWriter(new FileWriter(f))
    bw.write(swagger.getOpenAPI.toString)
    bw.close()
    val terraforms = OpenApiProcessor.processAllClasses(swagger.getOpenAPI, types)
    terraforms.foreach {
      case (terraform: ScalaSwaggerTemplate, baseType: String) =>
        writeFiles(terraform, baseType)
    }
  }

  def writeFiles(sumoSwaggerTemplate: ScalaSwaggerTemplate, baseType: String) = {
    val genSumoClass = SumoTerraformClassFileGenerator(sumoSwaggerTemplate)
    val terraformTypeName = removeCamelCase(baseType)

    genSumoClass.writeToFile(resourcesDirectory + s"sumologic_$terraformTypeName.go")

    val genResource = TerraformResourceFileGenerator(sumoSwaggerTemplate)
    genResource.writeToFile(resourcesDirectory + s"resource_sumologic_$terraformTypeName.go")

    val genTest = AcceptanceTestFileGenerator(sumoSwaggerTemplate, baseType)
    genTest.writeToFile(resourcesDirectory + s"resource_sumologic_${terraformTypeName}_test.go")

    val genDocs = TerraformDocsGenerator(sumoSwaggerTemplate, baseType)
    genDocs.writeToFile(resourcesDirectory + s"$terraformTypeName.html.markdown")
  }

  def ensureDirectories(): Unit = {
    val directory = new File(targetDirectory)
    if (!directory.exists) {
      directory.mkdir
    }
    val resourcesDirectoryFolder = new File(resourcesDirectory)
    if (!resourcesDirectoryFolder.exists) {
      resourcesDirectoryFolder.mkdir
    }
  }
}
