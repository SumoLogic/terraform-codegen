package com.sumologic.terraform_generator

import java.nio.file.{Files, Paths}

import org.scalatest.BeforeAndAfterAll

import scala.io.Source

class TerraformGeneratorTest
  extends UnitSpec
    with BeforeAndAfterAll {

  private val yaml = getClass.getResource("/pet.yml").getPath
  private val goTestFileName = "resource_sumologic_pet_test.go"
  private val goResourceFileName = "resource_sumologic_pet.go"
  private val goApiFileName = "sumologic_pet.go"

  private val expectedOutputDir = getClass.getResource("/expected/").getPath
  // TODO fix TerraformGenerator to take output dir as arg
  // private val outputDir = {
  //   val outputPath = s"${getClass.getResource("/").getPath}output/"
  //   Files.createDirectories(Paths.get(outputPath))
  //   outputPath
  // }
  private val outputDir = getClass.getResource("/").getPath + "../resources/"

  override def beforeAll(): Unit = {
    val args = Array(yaml)
    TerraformGenerator.main(args)
  }


  "TerraformGenerator" should {

    "generate api go file" in {
      compare(expectedOutputDir + goApiFileName, outputDir + goApiFileName)
    }

    "generate resource go file" in {
      compare(expectedOutputDir + goResourceFileName, outputDir + goResourceFileName)
    }

    "generate test go file" in {
      compare(expectedOutputDir + goTestFileName, outputDir + goTestFileName)
    }
  }

  def compare(f1: String, f2: String): Unit = {
    val file1 = Source.fromFile(f1)
    val file2 = Source.fromFile(f2)

    try {
      val it1 = file1.getLines().filterNot(_.trim.isEmpty)
      val it2 = file2.getLines().filterNot(_.trim.isEmpty)

      withClue("LOC (doesn't include empty lines) are not equal") {
        it1.size should be(it2.size)
      }

      while (it1.hasNext) {
        val l1 = it1.next()
        val l2 = it2.next()
        val minifiedL1 = l1.trim.replaceAll(" *\t*", "")
        val minifiedL2 = l2.trim.replaceAll(" *\t*", "")
        withClue(s"Files differ. Expected: '$l1', Got: '$l2'\n") {
          minifiedL1 should be(minifiedL2)
        }
      }
    } finally {
      file1.close()
      file2.close()
    }
  }

  def readFile(fileName: String): Iterator[String] = {
    val file = Source.fromFile(fileName)
    try {
      file.getLines
    } finally {
      file.close()
    }
  }
}