package com.sumologic.terraform_generator

import java.io.{BufferedWriter, File, FileWriter}

import com.sumologic.terraform_generator.utils.SumoTerraformUtils
import com.sumologic.terraform_generator.writer.{SumoTerraformClassFileGenerator, SumoTerraformResourceFileGenerator}
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions

object TerraformGenerator {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      // throw some error
    }
    val inputFile = args(0)
    if (args.size > 1) {
      println(s"You want to generate these types: ${args.drop(1).toString}")
    } else {
      println("You want to generate for all types")
    }

    val parseOpts = new ParseOptions()
    parseOpts.setResolve(true)
    parseOpts.setResolveCombinators(true)
    val swagger = new OpenAPIParser().readLocation(inputFile, null, parseOpts)
    val baseType = "extractionRule"
    val f = new File("openapi_schema.txt")
    val bw = new BufferedWriter(new FileWriter(f))
    bw.write(swagger.getOpenAPI.toString)
    bw.close()
    val terraform = SumoTerraformUtils.processClass(swagger.getOpenAPI, baseType)
    //println(terraform.terraformify())
    //freakOut("\n//--**--**--**--**--**--**BEGIN--**--**--**--**--**--**--**--\n\n")
    val genSumoClass = SumoTerraformClassFileGenerator(terraform)
    genSumoClass.writeToFile(s"sumologic_${baseType}.go")

    val genResource = SumoTerraformResourceFileGenerator(terraform)
    genResource.writeToFile(s"resource_sumologic_${baseType}.go")

    //val genDataSource = SumoTerraformDataSourceFileGenerator(terraform)
    //genDataSource.writeToFile(s"data_source_sumologic_${baseType}.go")
    //freakOut("\n//--**--**--**--**--**--**DONE--**--**--**--**--**--**--**--\n\n")

    //val genProvider = SumoProviderGenerator(terraform)
    //genProvider.writeToFile("provider.go")
  }
}
