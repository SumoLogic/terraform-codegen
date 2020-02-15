package com.sumologic.terraform_generator

import java.io.{BufferedWriter, File, FileWriter}

import com.sumologic.terraform_generator.objects.SumoSwaggerTemplate
import com.sumologic.terraform_generator.utils.SumoTerraformUtils
import com.sumologic.terraform_generator.writer.{SumoTerraformClassFileGenerator, SumoTerraformResourceFileGenerator}
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions

object TerraformGenerator {
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
    val swagger = new OpenAPIParser().readLocation(inputFile, null, parseOpts)
    val f = new File("openapi_schema.txt")
    val bw = new BufferedWriter(new FileWriter(f))
    bw.write(swagger.getOpenAPI.toString)
    bw.close()
    if (!types.isEmpty) {
      types.foreach {
        baseType =>
          val terraform = SumoTerraformUtils.processClass(swagger.getOpenAPI, baseType)
          val genSumoClass = SumoTerraformClassFileGenerator(terraform)
          genSumoClass.writeToFile(s"sumologic_${baseType}.go")

          val genResource = SumoTerraformResourceFileGenerator(terraform)
          genResource.writeToFile(s"resource_sumologic_${baseType}.go")

        //val genDataSource = SumoTerraformDataSourceFileGenerator(terraform)
        //genDataSource.writeToFile(s"data_source_sumologic_${baseType}.go")

        //val genProvider = SumoProviderGenerator(terraform)
        //genProvider.writeToFile("provider.go")
      }
    } else {
      val terraforms = SumoTerraformUtils.processAllClasses(swagger.getOpenAPI)
      terraforms.foreach {
        case (terraform: SumoSwaggerTemplate, baseType: String) =>
          val genSumoClass = SumoTerraformClassFileGenerator(terraform)
          genSumoClass.writeToFile(s"sumologic_${baseType}.go")

          val genResource = SumoTerraformResourceFileGenerator(terraform)
          genResource.writeToFile(s"resource_sumologic_${baseType}.go")

        //val genDataSource = SumoTerraformDataSourceFileGenerator(terraform)
        //genDataSource.writeToFile(s"data_source_sumologic_${baseType}.go")

        //val genProvider = SumoProviderGenerator(terraform)
        //genProvider.writeToFile("provider.go")
      }
    }
  }
}
