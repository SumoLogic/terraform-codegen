package com.sumologic.terraform_generator.writer

import java.io.{BufferedWriter, File, FileWriter}

import com.sumologic.terraform_generator.objects.SumoSwaggerSupportedOperations.crud
import com.sumologic.terraform_generator.utils.SumoTerraformPrinter.maybePrint
import com.sumologic.terraform_generator.utils.SumoTerraformUtils._
import com.sumologic.terraform_generator.objects.{SumoSwaggerEndpoint, SumoSwaggerTemplate, SumoSwaggerType, SumoTerraformEntity}

abstract class SumoTerraformFileGenerator(terraform: SumoSwaggerTemplate) {
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

case class SumoTerraformClassFileGenerator(terraform: SumoSwaggerTemplate)
  extends SumoTerraformFileGenerator(terraform: SumoSwaggerTemplate) {
  def generate(): String = {
    maybePrint(s"..............................SumoSwaggerTemplate: [${terraform.sumoSwaggerClassName}]..............................")
    // This should be a collection of all the types that are used for the list of endpoints
    val typesUsed: Set[SumoSwaggerType] = terraform.getAllTypesUsed()

    val intro = s"package $packageName\n\n" +
      // TODO Figure out how to variablize imports:
      "import (\n" +
      "  \"encoding/json\"\n" +
      "  \"fmt\"\n" +
      ")\n"

    val endpoints = terraform.supportedEndpoints.map {
      case endpoint: SumoSwaggerEndpoint =>
        endpoint.terraformify() + "\n" // + "\n//--**--**--**--**--**--**--**--**--**--**--**--**--**--\n\n"
    }.mkString("")

    val types = typesUsed.map {
      case stype: SumoSwaggerType =>
        stype.terraformify() + "\n" // + "\n//--##--##--##--##--##--##--##--##--##--##--##--##--##--##\n\n"
    }.mkString("")

    // TODO
    // TODO 1. Get validators to be called and returned error automatically to replace things like:
    /*
      if resourceData.Id() == "" {
    createdRole, err := client.createRole(role)


    or
    id := resourceData.Get("id").(string)
    return client.deleteRole(id)


    Also replace call sites in client.crudRole(...) to do the conversion automatically at call sites after validation

     */


    // This is useless here, there is no resourceData references in this class, either we need bodyParam to mainClass converters
    //or we need to do the conversions on the resource or data source classes
    // or we can ask the customer for body param type objects as terraform resourceData objects for
    // TODO

    // maybe best is for now to have Role To CreateRole converters
    // TODO


    s"// ---------- BEGIN ${terraform.sumoSwaggerClassName} ----------\n" + intro +
      "\n// ---------- ENDPOINTS ---------- \n\n" + endpoints +
      "\n// ---------- TYPES ----------\n" + types +
      //"\n// ---------- CONVERTERS ----------\n" + complexTypesConverters +
      "\n// ---------- END ----------\n"
  }
}

case class SumoTerraformDataSourceFileGenerator(terraform: SumoSwaggerTemplate)
  extends SumoTerraformFileGenerator(terraform: SumoSwaggerTemplate) {
  def generate(): String = {
    val pre = """package sumologic
                |
                |import (
                |  "errors"
                |  "fmt"
                |  "github.com/hashicorp/terraform/helper/schema"
                |  "log"
                |)
                |""".stripMargin

    val dataSourceFunction = SwaggerDataSourceFunctionGenerator(terraform.getMainObjectClass())
    pre + seperatorLine() + terraform.getDataSourceFuncMappings() + seperatorLine() +
      dataSourceFunction.terraformify() + seperatorLine("Converters") +
      getTerraformObjectToResourceDataConverter(terraform.getMainObjectClass())
  }
}

case class SumoTerraformResourceFileGenerator(terraform: SumoSwaggerTemplate)
  extends SumoTerraformFileGenerator(terraform: SumoSwaggerTemplate) {
  def generate(): String = {
    val pre = """package sumologic
                |
                |import (
                |  // "errors"
                |  "fmt"
                |  "log"
                |  "github.com/hashicorp/terraform/helper/schema"
                |)
                |""".stripMargin

    val mappingSchema = terraform.getResourceFuncMappings()

    val ops: String = terraform.supportedEndpoints.map {
      endpoint: SumoSwaggerEndpoint =>
        val gen = SwaggerResourceFunctionGenerator(endpoint, terraform.getMainObjectClass())
        gen.terraformify()
    }.mkString(seperatorLine())

    val complexTypesConverters = terraform.getAllRequestBodyTypesUsed().filter(_.isCompositeType()).map {
      case stype: SumoSwaggerType =>
        getTerraformResourceDataToObjectConverter(stype, false)
    }.mkString(seperatorLine())

    val resourceDataSetters = terraform.getAllRequestBodyTypesUsed().filter(_.isCompositeType()).map {
      case stype: SumoSwaggerType =>
        getTerraformObjectToResourceDataConverter(stype)
    }.mkString(seperatorLine())

    val varPrefix = varNameFromTypeName(terraform.getMainObjectClass.name)

    val converters = seperatorLine("Converters") +
      getTerraformResourceDataToObjectConverter(terraform.getMainObjectClass(), true) +
      seperatorLine() +
      getTerraformObjectToResourceDataConverter(terraform.getMainObjectClass()).replace(s"${varPrefix}ToResourceData", s"${varPrefix}ToResourceDataForResource") +
      seperatorLine() +
      complexTypesConverters +
      seperatorLine() +
      resourceDataSetters

    pre + seperatorLine() + mappingSchema + seperatorLine() + ops + seperatorLine() +
      converters + seperatorLine()
  }
}

case class SwaggerDataSourceFunctionGenerator(mainClass: SumoSwaggerType) extends SumoTerraformEntity {
  def getTerraformDataResourceSetters(propName: String, objName: String): String = {
    s"""resourceData.Set("$propName", $objName.$propName)""".stripMargin
  }

  override def terraformify(): String = {
    val className = mainClass.name
    val objName = varNameFromTypeName(className)
    /*
    val setters = mainClass.props.map {
      prop: SumoSwaggerObject =>
        val name = prop.getName()
        s"""resourceData.Set("$name", $objName.$name)""".stripMargin
    }.mkString("\n")
    */

    val setter = getTerraformObjectToResourceDataConverterFuncCall(mainClass)
    // Assuming id is of type string
    s"""
       |func dataSourceSumologic${className}Get(resourceData *schema.ResourceData, meta interface{}) error {
       |  client := meta.(*Client)
       |
      |  var $objName *${className}
       |  var err error
       |  if id, ok := resourceData.GetOk("id"); ok {
       |    $objName, err = client.get${className}(id.(string))
       |    if err != nil {
       |      return fmt.Errorf("SumologicTerraformError: ${className} with id %s not found: %v", id, err)
       |    }
       |  } else {
       |      return errors.New("SumologicTerraformError: ${className} object Id is required")
       |    }
       |
      |  resourceData.SetId($objName.id)
       |  $setter
       |
      |  log.Printf("[DEBUG] SumologicTerraformDebug DataSource ${className} : retrieved %v", $objName)
       |  return nil
       |}""".stripMargin
  }
}


case class SwaggerResourceFunctionGenerator(endpoint: SumoSwaggerEndpoint, mainClass: SumoSwaggerType) extends SumoTerraformEntity {

  // TODO: This is gross, generalize if possible
  def generateResourceFunctionGET(): String = {
    val className = mainClass.name
    val objName = varNameFromTypeName(className)
    /*
    val setters = mainClass.props.map {
      prop: SumoSwaggerObject =>
        val name = prop.getName()
        s"""resourceData.Set("$name", $objName.$name)""".stripMargin
    }.mkString("\n")
    */
    val setter = getTerraformObjectToResourceDataConverterFuncCall(mainClass)
    val vars = getArgsListAsVariableAssignmentsFromDataSourceSchema(endpoint.parameters).mkString("\n")
    val argsList = getArgsListForFuncCall(endpoint.parameters).map {
      param =>
        if (param.toLowerCase.contains("id")) {
          "resourceData.Id()"
        } else {
          param
        }
    }.mkString(", ")

    // Assuming id is of type string
    s"""
       |func resourceSumologic${className}Get(resourceData *schema.ResourceData, meta interface{}) error {
       |  client := meta.(*Client)
       |
       |  $vars
       |
       |  $objName, err := client.get${className}(${argsList.replace("id", "idExists.(string)")})
       |  if err != nil {
       |    return err
       |  }
       |
       |  // var $objName *${className}
       |  // var err error
       |  //if id, ok := resourceData.Id(); ok {
       |  id := resourceData.Id()
       |  $objName, err = client.get${className}(id)
       |  if err != nil {
       |    return fmt.Errorf("SumologicTerraformError: ${className} with id %s not found: %v", id, err)
       |  }
       |  //} else {
       |   // return errors.New("SumologicTerraformError: ${className} object Id is required")
       |  //}
       |
       |  resourceData.SetId($objName.Id)
       |  ${setter.replace(s"${objName}ToResourceData", s"${objName}ToResourceDataForResource")}
       |
       |  log.Printf("[DEBUG] SumologicTerraformDebug DataSource ${className} : retrieved %v", $objName)
       |  return nil
       |}""".stripMargin
  }

  // TODO: This is gross, generalize if possible
  def generateResourceFunctionDELETE(): String = {
    val className = mainClass.name
    val objName = varNameFromTypeName(className)

    // TODO: I removed atoi(id)...
    // TODO , should it be
    s"""func resourceSumologic${className}Delete(resourceData *schema.ResourceData, meta interface{}) error {
       |  client := meta.(*Client)
       |
       |  return client.delete${className}(resourceData.Id())
       |}""".stripMargin

  }

  // TODO: This is gross, generalize if possible
  def generateResourceFunctionUPDATE(): String = {
    val className = mainClass.name
    val objName = varNameFromTypeName(className)

    val parameter = endpoint.parameters.filter(
      _.param.getName.toLowerCase.contains(objName.toLowerCase)).head.param.getName()

    val lowerCaseName = parameter.substring(0, 1).toLowerCase() + parameter.substring(1)

    // TODO: remove this drop and reverse
    val vars = getArgsListAsVariableAssignmentsFromDataSourceSchema(endpoint.parameters).reverse.drop(1).mkString("\n")
    val argsList = getArgsListForFuncCall(endpoint.parameters).map {
      param =>
        if (param.toLowerCase.contains("id")) {
          "resourceData.Id()"
        } else {
          param
        }
    }.mkString(", ")

    /* s"""
       |func resourceSumologic${className}Update(resourceData *schema.ResourceData, meta interface{}) error {
       |
      |  $vars
       | var update${className}Definition *Update${className}Definition
       | update${className}Definition, err := resourceDataToUpdate${className}Definition(resourceData)
       | if err != nil {
       |  return err
       | }
       |
      |  client := meta.(*Client)
       |  _, err = client.update${className}(${argsList.replace(s"update${className}Definition",
      s"*update${className}Definition").replace("id,", "idExists.(string),")})
       |
      |  if err != nil {
       |    return err
       |  }
       |
       |  update${className}DefinitionToResourceData(resourceData, update${className}Definition)
       |
      |  return resourceSumologic${className}Get(resourceData, meta)
       |}""".stripMargin */
    s"""
       |func resourceSumologic${className}Update(resourceData *schema.ResourceData, meta interface{}) error {
       |
      |  $vars
       | var ${lowerCaseName} *${parameter}
       | ${lowerCaseName}, err := resourceDataTo${parameter}(resourceData)
       | if err != nil {
       |  return err
       | }
       |
      |  client := meta.(*Client)
       |  _, err = client.update${className}(${argsList.replace(s"${lowerCaseName}",
      s"*${lowerCaseName}").replace("id,", "idExists.(string),")})
       |
      |  if err != nil {
       |    return err
       |  }
       |
       |  ${lowerCaseName}ToResourceData(resourceData, ${lowerCaseName})
       |
      |  return resourceSumologic${className}Get(resourceData, meta)
       |}""".stripMargin
  }

  // TODO: This is gross, generalize if possible
  def generateResourceFunctionCREATE(): String = {
    val className = mainClass.name
    val objName = varNameFromTypeName(className)

    val parameter = endpoint.parameters.filter(
      _.param.getName.toLowerCase.contains(objName.toLowerCase)).head.param.getName()

    val lowerCaseName = parameter.substring(0, 1).toLowerCase() + parameter.substring(1)

    s"""
       |func resourceSumologic${className}Create(resourceData *schema.ResourceData, meta interface{}) error {
       |  client := meta.(*Client)
       |  var ${lowerCaseName} *${parameter}
       |  ${lowerCaseName}, err := resourceDataTo${parameter}(resourceData)
       |  if err != nil {
       |    return err
       |  }
       |
       |  if resourceData.Id() == "" {
       |    created${className}, err := client.create${className}(*${lowerCaseName})
       |
       |    if err != nil {
       |      return err
       |    }
       |
       |    ${objName}ToResourceDataForResource(resourceData, created${className})
       |    resourceData.SetId(created${className}.Id)
       |    return resourceSumologic${className}Get(resourceData, meta)
       |  }
       |
       |  return resourceSumologic${className}Update(resourceData, meta)
       |
       |}""".stripMargin

  }

  // TODO: This is gross, generalize if possible
  def generateResourceFunctionLIST(): String = {
    val className = mainClass.name
    val objName = className.substring(0, 1).toLowerCase() + className.substring(1)

    s"""
       |func resourceSumologic${className}List(resourceData *schema.ResourceData, meta interface{}) error {
       |  client := meta.(*Client)
       |
       |  list${className}sResult, err := client.list${className}s()
       |
       |  if err != nil {
       |    return err
       |  }
       |
       |  return list${className}sResult
       |}""".stripMargin
  }

  // TODO: This is gross, generalize if possible
  override def terraformify(): String = {
    crud.find(_ + mainClass.name == endpoint.endpointName) match {
      case Some(opName) => this.getClass.getMethod("generateResourceFunction" + opName.toUpperCase()).invoke(this).toString
      case None => ""
    }
  }
}

case class SumoProviderGenerator(terraform: SumoSwaggerTemplate)
  extends SumoTerraformFileGenerator(terraform: SumoSwaggerTemplate) {
  def generate(): String = {
    val pre = """package main
                |
                |import (
                |  "fmt"
                |  "github.com/go-errors/errors"
                |  "log"
                |  "os"
                |
                |  "github.com/hashicorp/terraform/helper/mutexkv"
                |  "github.com/hashicorp/terraform/terraform"
                |  "github.com/hashicorp/terraform/helper/schema"
                |)
                |
                |const DefaultEnvironment = "us2"
                |
                |func Provider() terraform.ResourceProvider {
                |	defaultEnvironment := os.Getenv("SUMOLOGIC_ENVIRONMENT")
                |	if defaultEnvironment == "" {
                |		defaultEnvironment = DefaultEnvironment
                |	}
                |	log.Printf("[DEBUG] sumo default environment: %s", defaultEnvironment)
                |
                |	return &schema.Provider{
                |		Schema: map[string]*schema.Schema{
                |			"access_id": {
                |				Type:     schema.TypeString,
                |				Optional: true,
                |				Default:  os.Getenv("SUMOLOGIC_ACCESSID"),
                |			},
                |			"access_key": {
                |				Type:     schema.TypeString,
                |				Optional: true,
                |				Default:  os.Getenv("SUMOLOGIC_ACCESSKEY"),
                |			},
                |			"environment": {
                |				Type:     schema.TypeString,
                |				Optional: true,
                |				Default:  defaultEnvironment,
                |			},
                |		},
                |""".stripMargin

    val post = """    ConfigureFunc: providerConfigure,
                 |	}
                 |}
                 |
                 |var SumoMutexKV = mutexkv.NewMutexKV()
                 |
                 |func providerConfigure(d *schema.ResourceData) (interface{}, error) {
                 |	accessId := d.Get("access_id").(string)
                 |	accessKey := d.Get("access_key").(string)
                 |	environment := d.Get("environment").(string)
                 |
                 |	msg := ""
                 |	if accessId == "" {
                 |		msg = "sumologic provider: access_id should be set;"
                 |	}
                 |	if accessKey == "" {
                 |		msg = fmt.Sprintf("%s access_key should be set; ", msg)
                 |	}
                 |	if msg != "" {
                 |		if environment == DefaultEnvironment {
                 |			msg = fmt.Sprintf("%s make sure environment is set or that the default (%s) is appropriate", msg, DefaultEnvironment)
                 |		}
                 |		return nil, errors.New(msg)
                 |	}
                 |
                 |	return NewClient(
                 |		accessId,
                 |		accessKey,
                 |		environment,
                 |	)
                 |}
                |""".stripMargin

    val className = terraform.getMainObjectClass.name
    val lowerCaseClass = className.toLowerCase


    // TODO: make this a list for all classes
    val resourcesMap = s"""    ResourcesMap: map[string]*schema.Resource{
                         |			"sumologic_$lowerCaseClass":          resourceSumologic$className(),
                         |		},
                         |""".stripMargin

    val dataSourcesMap = s"""    DataSourcesMap: map[string]*schema.Resource{
                          |			"sumologic_$lowerCaseClass":          dataSourceSumologic$className(),
                          |		},
                          |""".stripMargin

    pre + resourcesMap + dataSourcesMap + post
  }
}
