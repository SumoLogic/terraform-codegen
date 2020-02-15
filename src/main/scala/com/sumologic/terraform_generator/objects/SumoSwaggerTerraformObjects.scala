package com.sumologic.terraform_generator.objects

import com.sumologic.terraform_generator.objects.SumoSwaggerSupportedOperations.crud
import com.sumologic.terraform_generator.utils.SumoTerraformPrinter.{freakOut, maybePrint}
import com.sumologic.terraform_generator.TerraformGeneratorHelper
import com.sumologic.terraform_generator.utils.{SumoTerraformSchemaTypes, SumoTerraformSupportedParameterTypes}

object SumoSwaggerSupportedOperations {
  final val CREATE = "create"
  final val GET = "get"
  final val UPDATE = "update"
  final val DELETE = "delete"
  final val LIST = "list"

  val crud = List(CREATE, GET, UPDATE, DELETE, LIST)
}

// scalastyle:off
abstract class SumoTerraformEntity {
  def indent = "    "
  def terraformify(): String = ""
}

abstract sealed class SumoSwaggerObject(name: String,
                                        objType: SumoSwaggerType,
                                        required: Boolean,
                                        defaultOpt: Option[AnyRef]) extends SumoTerraformEntity
  with TerraformGeneratorHelper {
  // TODO Assumption of NO collision without namespaces is probably wrong - should fix
  def getAllTypes(): List[SumoSwaggerType] = {
    List(objType) ++ objType.props.flatMap(_.getAllTypes())
  }

  def getName(): String = { name }
  def getType(): SumoSwaggerType = { objType }
  def getRequired(): Boolean = { required }

  def getAsTerraformFunctionArgument(): String
  // def getAsTerraformSchemaType(forUseInDateResource: Boolean): String

  def getAsTerraformSchemaType(forUseInDataResource: Boolean): String = {
    val schemaType = if (this.isInstanceOf[SumoSwaggerObjectArray]) {
      SumoTerraformSchemaTypes.swaggerTypeToTerraformSchemaType("array")
    } else {
      SumoTerraformSchemaTypes.swaggerTypeToTerraformSchemaType(objType.name)
    }
    val requiredTxt = if (required) { // TODO: check why Frank did it this way
      "Required: true"
    } else {
      "Optional: true"
    }
    val specifics = if (forUseInDataResource) {
      "Computed: true"
    } else {
      defaultOpt match { // This probably won't work for composite types
        case Some(defaultValue) => "ForceNew: false,\nDefault: " + defaultValue.toString
        case None => "ForceNew: false"
      }
    }

    val elementType = if (this.isInstanceOf[SumoSwaggerObjectArray]) {
      s"Elem:  &schema.Schema{Type: ${SumoTerraformSchemaTypes.swaggerTypeToTerraformSchemaType(objType.name)}},\n"
    } else {
      ""
    }
    val noCamelCaseName = removeCamelCase(name)
    //s""""$name": {\n  Type: $schemaType,\n  $requiredTxt,\n  $specifics\n}"""
    "\"" + noCamelCaseName + "\"" + s": {\n  Type: $schemaType,\n  $requiredTxt,\n  $specifics,\n  $elementType}"
  }
}

case class SumoSwaggerObjectSingle(name: String,
                                   objType: SumoSwaggerType,
                                   required: Boolean,
                                   defaultOpt: Option[AnyRef]) extends
  SumoSwaggerObject(name: String, objType: SumoSwaggerType, required: Boolean, defaultOpt: Option[AnyRef]) {
  override def terraformify(): String = {
    val req = if (required) {
      ""
    } else {
      ",omitempty"
    }
    s"${name.capitalize} ${objType.name} " + "`" + "json:\"" + name + req + "\"" + "`" + "\n"
  }

  def getAsTerraformFunctionArgument(): String = {
    s"$name ${objType.name}"
  }
}

case class SumoSwaggerObjectArray(name: String,
                                  objType: SumoSwaggerType,
                                  required: Boolean,
                                  defaultOpt: Option[AnyRef]) extends
  SumoSwaggerObject(name: String, objType: SumoSwaggerType, required: Boolean, defaultOpt: Option[AnyRef]) {
  override def terraformify(): String = {
    val req = if (required) {
      ""
    } else {
      ",omitempty"
    }

    s"${name.capitalize} []${objType.name} " + "`" + "json:\"" + name + req + "\"" + "`" + "\n"
  }

  def getAsTerraformFunctionArgument(): String = {
    s"$name []${objType.name}"
  }


}

case class SumoSwaggerType(name: String, props: List[SumoSwaggerObject] = List[SumoSwaggerObject]())
  extends SumoTerraformEntity {
  override def terraformify(): String = {
    if (props.isEmpty) {
      ""
    } else {
      val terraProps = props.map(indent + _.terraformify())
      if (name.toLowerCase == "errorresponse" || name.toLowerCase == "errordescription") {
        ""
        // s"type SwaggerErrorResponse struct {\n" + terraProps.mkString("") + "}\n"
      } else {
        s"type $name struct {\n" + terraProps.mkString("") + "}\n"
      }
    }
  }

  def getAsTerraformSchemaType(): String = {
    if (props.isEmpty) {
      ""
    } else {
      val terraProps = props.map(indent + _.terraformify())
      s"type $name struct {\n" + terraProps.mkString("") + "}\n"
    }
  }

  def isCompositeType(): Boolean = {
    !props.isEmpty
  }
}


case class SumoSwaggerResponse(respTypeName: String, respTypeOpt: Option[SumoSwaggerType]) extends SumoTerraformEntity {
  override def terraformify(): String = {
    if(respTypeOpt.isEmpty) {
      "empty_resp_body"
    } else {
      respTypeOpt.get.name
    }
  }
}

case class SumoSwaggerParameter(paramType: String, param: SumoSwaggerObject) extends SumoTerraformEntity {
  override def terraformify(): String = {
    s"${param.getName()} ${param.getType().name}"
  }

  def toTerraformFuncArg(): String = {
    param.getAsTerraformFunctionArgument()
  }
}

case class SumoSwaggerEndpoint(endpointName: String,
                               path: String,
                               httpMethod: String,
                               parameters: List[SumoSwaggerParameter],
                               responses: List[SumoSwaggerResponse]) extends SumoTerraformEntity {

  case class ResponseProps(declReturnType: String, httpClientReturnType: String, responseVarDecl: String, unmarshal: String)
  def getReturnTypesBasedOnRespone(): ResponseProps = {
    val respBodyTypeOpt = this.responses.filter(_.respTypeName != "default").head.respTypeOpt
    respBodyTypeOpt match {
      case Some(respType) =>
        val returnHandlingPart =
          """if err != nil {
            |      return nil, err
            |    }
            |
            |    err = json.Unmarshal(responseBody, &response)
            |
            |    if err != nil {
            |        return nil, err
            |    }
            |
            |    return &response, nil""".stripMargin
        if (httpMethod.toLowerCase == "get") {
          ResponseProps(s"(*${respType.name}, error)", "responseBody, _, err", s"var response ${respType.name}\n", returnHandlingPart)
        } else {
          ResponseProps(s"(*${respType.name}, error)", "responseBody, err", s"var response ${respType.name}\n", returnHandlingPart)
        }
      case None =>
        val returnHandlingPart =
          """return err""".stripMargin
        ResponseProps("error", "_, err", "", returnHandlingPart)
    }
  }

  override def terraformify(): String = {
    maybePrint(s"..............................SumoSwaggerEndpoint: [$endpointName => $path ($httpMethod)]..............................")
    import com.sumologic.terraform_generator.utils.SumoTerraformUtils._

    val bodyParamOpt = this.parameters.find(_.paramType == SumoTerraformSupportedParameterTypes.BodyParameter)
    val sprintfArg = makeTerraformUrlFormatForSprintf(this.path, this.parameters).replaceFirst("/", "")
    val responseProps = getReturnTypesBasedOnRespone()

    val args = makeArgsListForDecl(this.parameters)
    val httpClientCall = makeTerraformHttpClientRequestString(this.httpMethod, bodyParamOpt)
    s"""
       |func (httpClient *Client) ${this.endpointName}($args) ${responseProps.declReturnType} {
       |    url := fmt.Sprintf($sprintfArg)
       |    ${responseProps.responseVarDecl}
       |    ${responseProps.httpClientReturnType} := httpClient.$httpClientCall
       |    ${responseProps.unmarshal}
       |}
       |""".stripMargin
  }
}

case class SumoSwaggerTemplate(sumoSwaggerClassName: String,
                               supportedEndpoints: List[SumoSwaggerEndpoint]) extends SumoTerraformEntity {
  def getAllTypesUsed(): Set[SumoSwaggerType] = {
    freakOut(s"THOMASKAO SUPPORTEDENDPOINTS: ${supportedEndpoints.map(_.endpointName).toString()}")
    val responsesProps = supportedEndpoints.flatMap {
      endpoint =>
        endpoint.responses.flatMap {
          response =>
            response.respTypeOpt.map {
              respType => respType.props.flatMap(_.getAllTypes())
            }
        }
    }.flatten

    freakOut(s"THOMASKAO RESPONSES: ${responsesProps.map(_.name).toString()}")

    val parameterProps = supportedEndpoints.flatMap {
      endpoint =>
        endpoint.parameters.flatMap {
          parameter => parameter.param.getAllTypes
        }
    }

    freakOut(s"THOMASKAO PARAMETERS: ${parameterProps.map(_.name).toString()}")

    val endpointsSet = supportedEndpoints.flatMap { endpoint =>
      endpoint.parameters.flatMap(_.param.getAllTypes()) ++
        endpoint.responses.flatMap(_.respTypeOpt) ++
      responsesProps ++
      parameterProps

    }.toSet

    freakOut(s"THOMASKAO ENDPOINTS: ${endpointsSet.map(_.name).toString()}")

    endpointsSet
  }

  def getUpdateAndCreateRequestBodyType(): List[SumoSwaggerType] = {
    val endpoints = supportedEndpoints.filter { op: SumoSwaggerEndpoint =>
      op.endpointName.equalsIgnoreCase(SumoSwaggerSupportedOperations.UPDATE + sumoSwaggerClassName) ||
        op.endpointName.equalsIgnoreCase(SumoSwaggerSupportedOperations.CREATE + sumoSwaggerClassName)
    }

    val types = endpoints.flatMap {
      endpoint => endpoint.parameters.filter(_.paramType == SumoTerraformSupportedParameterTypes.BodyParameter)
        .flatMap(_.param.getAllTypes())
    }

    types.filter(_.name.toUpperCase.contains(sumoSwaggerClassName.toUpperCase()))
  }

  def getAllRequestBodyTypesUsed(): Set[SumoSwaggerType] = {
    supportedEndpoints.flatMap { endpoint =>
      endpoint.parameters.filter(_.paramType == SumoTerraformSupportedParameterTypes.BodyParameter).
        flatMap(_.param.getAllTypes())
    }.toSet
  }

  def getDataSourceFuncMappings(): String = {
    val funcMappings: String = getFunctionMappings(
      List[String](SumoSwaggerSupportedOperations.GET)).mkString(",\n").concat(",")

    val mainClass = getMainObjectClass()

    val mainClassProps = mainClass.props.map {
      case sobj: SumoSwaggerObject =>
        sobj.getAsTerraformSchemaType(true)
    }.mkString(",\n").concat(",")

    // TODO: Find out from Frank Reno why destroy check is added and is it common practice?
    s"""func dataSourceSumologic$sumoSwaggerClassName() *schema.Resource {
       |  return &schema.Resource{
       |    $funcMappings
       |    Schema: map[string]*schema.Schema{
       |      $mainClassProps
       |    },
       |  }
       |}""".stripMargin
  }


  def getFunctionName(opName: String, prefix: String): String = {
    val camelOp = opName.substring(0, 1).toUpperCase() + opName.substring(1)
    s"$prefix$sumoSwaggerClassName$camelOp"
  }

  def getFunctionMappings(interestedInOps: List[String]): List[String] = {
    interestedInOps.map {
      op: String =>
        val camelOp = op.substring(0, 1).toUpperCase() + op.substring(1)
        if (camelOp == "Get") {
          s"Read: ${getFunctionName(op, "resourceSumologic")}"
        } else {
          s"$camelOp: ${getFunctionName(op, "resourceSumologic")}"
        }
    }
  }

  def getMainObjectClass(): SumoSwaggerType = {
    val typesUsed: Set[SumoSwaggerType] = getAllTypesUsed()

    typesUsed.find(_.name.toUpperCase == sumoSwaggerClassName.toUpperCase()).getOrElse {
      freakOut("WTF??????WTF??????WTF??????WTF?????? => NO MAIN CLASS for " + sumoSwaggerClassName)
      throw new RuntimeException("This should not happen in getMainObjectClass ")
    }
  }

  def getResourceFuncMappings(): String = {
    // TODO Each type used needs to be generated somewhere for this to work, for now...
    // ... hoping that this is all basic types

    val funcMappings: String = getFunctionMappings(crud).mkString(",\n").concat(",")

    // TODO This assumption is too optimistic
    val classes = getUpdateAndCreateRequestBodyType()

    val classesProps = classes.flatMap(_.props.filter(prop => !prop.getName().toLowerCase.contains("created") &&
      !prop.getName().toLowerCase.contains("modified") && !prop.getName().toLowerCase.contains("system") &&
      !prop.getName().toLowerCase.equals("id"))).filter(_.getRequired()).toSet

    val propsObjects = classesProps.map {
      sumoSwaggerObject: SumoSwaggerObject =>
        sumoSwaggerObject.getAsTerraformSchemaType(false)
    }.toList.toSet.mkString(",\n").concat(",")

    s"""func resourceSumologic$sumoSwaggerClassName() *schema.Resource {
       |  return &schema.Resource{
       |    $funcMappings
       |    Importer: &schema.ResourceImporter{
       |      State: schema.ImportStatePassthrough,
       |    },
       |
      |    Schema: map[string]*schema.Schema{
       |      $propsObjects
       |      "destroy": {
       |        Type:     schema.TypeBool,
       |        Optional: true,
       |        ForceNew: false,
       |        Default:  true,
       |      },
       |    },
       |  }
       |}""".stripMargin
  }

}