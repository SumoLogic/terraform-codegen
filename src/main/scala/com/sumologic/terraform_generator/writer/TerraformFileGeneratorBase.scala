package com.sumologic.terraform_generator.writer

import java.io.{BufferedWriter, File, FileWriter}

import com.sumologic.terraform_generator.StringHelper
import com.sumologic.terraform_generator.objects.TerraformResource

abstract class TerraformFileGeneratorBase(terraform: TerraformResource) extends StringHelper {
  def writeToFile(filePath: String): Unit = {
    val text = generate()
    val file = new File(filePath)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
  }


  //TODO pass in the file or output stream
  def generate(): String
}
