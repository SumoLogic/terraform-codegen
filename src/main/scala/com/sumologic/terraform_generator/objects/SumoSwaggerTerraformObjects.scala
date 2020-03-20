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
  final val EXISTS = "exists"

  val crud = List(CREATE, GET, UPDATE, DELETE, EXISTS)
}

object ForbiddenGoTerms {
  final val TYPE = "type"

  val forbidden = List(TYPE)
}

// scalastyle:off
abstract class SumoTerraformEntity extends TerraformGeneratorHelper {
  def indent = "    "
  def terraformify(): String = ""
}

abstract sealed class SumoSwaggerObject(name: String,
                                        objType: SumoSwaggerType,
                                        required: Boolean,
                                        defaultOpt: Option[AnyRef],
                                        description: String,
                                        example: String) extends SumoTerraformEntity
  with TerraformGeneratorHelper {
  // TODO Assumption of NO collision without namespaces is probably wrong - should fix
  def getAllTypes(): List[SumoSwaggerType] = {
    List(objType) ++ objType.props.flatMap(_.getAllTypes())
  }

  def getName(): String = { name }
  def getType(): SumoSwaggerType = { objType }
  def getRequired(): Boolean = { required }
  def getDescription(): String = { description }
  def getExample(): String = { example }

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
      s"""Elem:  &schema.Schema{
         |            Type: ${SumoTerraformSchemaTypes.swaggerTypeToTerraformSchemaType(objType.name)},
         |           },""".stripMargin
    } else {
      ""
    }
    val noCamelCaseName = removeCamelCase(name)
    //s""""$name": {\n  Type: $schemaType,\n  $requiredTxt,\n  $specifics\n}"""
    "\"" + noCamelCaseName + "\"" + s": {\n           Type: $schemaType,\n          $requiredTxt,\n           $specifics,\n           $elementType\n         }"
  }
}

case class SumoSwaggerObjectSingle(name: String,
                                   objType: SumoSwaggerType,
                                   required: Boolean,
                                   defaultOpt: Option[AnyRef],
                                   description: String,
                                   example: String) extends
  SumoSwaggerObject(name: String, objType: SumoSwaggerType, required: Boolean, defaultOpt: Option[AnyRef], description, example) {
  override def terraformify(): String = {
    val req = if (required && name.toLowerCase != "id") {
      ""
    } else {
      ",omitempty"
    }
    if (name.toLowerCase == "id") {
      s"${name.toUpperCase} ${objType.name} " + "`" + "json:\"" + name + req + "\"" + "`" + "\n"
    } else {
      s"${name.capitalize} ${objType.name} " + "`" + "json:\"" + name + req + "\"" + "`" + "\n"
    }
  }

  def getAsTerraformFunctionArgument(): String = {
    s"$name ${objType.name}"
  }
}

case class SumoSwaggerObjectArray(name: String,
                                  objType: SumoSwaggerType,
                                  required: Boolean,
                                  defaultOpt: Option[AnyRef],
                                  description: String,
                                  example: String) extends
  SumoSwaggerObject(name: String, objType: SumoSwaggerType, required: Boolean, defaultOpt: Option[AnyRef], description, example) {
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
          this.httpMethod.toLowerCase match {
            case "get" =>
              s"""
                |    err = json.Unmarshal(data, &${respType.name.substring(0,1).toLowerCase + respType.name.substring(1)})
                |
                |    if err != nil {
                |        return nil, err
                |    }
                |
                |    return &${respType.name.substring(0,1).toLowerCase + respType.name.substring(1)}, nil""".stripMargin
            case "post" =>
              s"""
                |    err = json.Unmarshal(data, &created${respType.name})
                |    if err != nil {
                |        return "", err
                |    }
                |
                |    return created${respType.name}.ID, nil""".stripMargin
            case "put" =>
              s"""
                |    ${respType.name.substring(0,1).toLowerCase + respType.name.substring(1)}.ID = ""
                |
                |    _, err := s.Put(url, ${respType.name.substring(0,1).toLowerCase + respType.name.substring(1)})
                |    return err""".stripMargin
            case "delete" =>
              """
                |return err""".stripMargin
            case _ =>
              ""
          }
        if (httpMethod.toLowerCase == "get") {
          ResponseProps(s"(*${respType.name}, error)", "responseBody, _, err", s"var ${respType.name.substring(0,1).toLowerCase + respType.name.substring(1)} ${respType.name}\n", returnHandlingPart)
        } else {
          if (httpMethod.toLowerCase == "post") {
            ResponseProps(s"(string, error)", "responseBody, err", s"var created${respType.name} ${respType.name}\n", returnHandlingPart)
          } else {
            ResponseProps(s"error", "responseBody, err", "", returnHandlingPart)
          }
        }
      case None =>
        val returnHandlingPart =
          """return err""".stripMargin
        ResponseProps("error", "_, err", "", returnHandlingPart)
    }
  }

  def getUrlCallBasedOnHttpMethod(url: String): String = {
    val taggedResource = if (this.httpMethod.toLowerCase != "delete") {
      this.responses.filter {
        response => response.respTypeName != "default" && response.respTypeName != "204"
      }.head.respTypeOpt.get.name
    } else {
      ""
    }
    httpMethod.toLowerCase match {
      case "get" =>
        s"""
           |  data, _, err := s.Get(fmt.Sprintf($url))
           |  if err != nil {
           |		return nil, err
           |	}
           |	if data == nil {
           |		return nil, nil
           |	}
           |""".stripMargin
      case "post" =>
        s"""
           |  data, err := s.Post($url, ${taggedResource.substring(0,1).toLowerCase + taggedResource.substring(1)})
           |  if err != nil {
           |		return "", err
           |	}
           |""".stripMargin
      case "put" =>
        val fixedUrl = url.replace("id", s"${taggedResource.substring(0,1).toLowerCase + taggedResource.substring(1)}.ID")
        s"""url := fmt.Sprintf($fixedUrl)"""
      case "delete" =>
        s"""_, err := s.Delete(fmt.Sprintf($url))"""
      case _ =>
        ""
    }
  }

  override def terraformify(): String = {
    import com.sumologic.terraform_generator.utils.SumoTerraformUtils._

    val bodyParamOpt = this.parameters.find(_.paramType == SumoTerraformSupportedParameterTypes.BodyParameter)
    val sprintfArg = makeTerraformUrlFormatForSprintf(this.path, this.parameters).replaceFirst("/", "")
    val responseProps = getReturnTypesBasedOnRespone()
    val urlCall = getUrlCallBasedOnHttpMethod(sprintfArg)

    val args = makeArgsListForDecl(this.parameters)
    val httpClientCall = makeTerraformHttpClientRequestString(this.httpMethod, bodyParamOpt)
    s"""
       |func (s *Client) ${this.endpointName.capitalize}($args) ${responseProps.declReturnType} {
       |    $urlCall
       |    ${responseProps.responseVarDecl}
       |    ${responseProps.unmarshal}
       |}
       |""".stripMargin
  }
}

case class SumoSwaggerTemplate(sumoSwaggerClassName: String,
                               supportedEndpoints: List[SumoSwaggerEndpoint]) extends SumoTerraformEntity {
  def getAllTypesUsed(): Set[SumoSwaggerType] = {
    val responsesProps = supportedEndpoints.flatMap {
      endpoint =>
        endpoint.responses.flatMap {
          response =>
            response.respTypeOpt.map {
              respType => respType.props.flatMap(_.getAllTypes())
            }
        }
    }.flatten

    val parameterProps = supportedEndpoints.flatMap {
      endpoint =>
        endpoint.parameters.flatMap {
          parameter => parameter.param.getAllTypes
        }
    }

    val endpointsSet = supportedEndpoints.flatMap { endpoint =>
      endpoint.parameters.flatMap(_.param.getAllTypes()) ++
        endpoint.responses.flatMap(_.respTypeOpt) ++
      responsesProps ++
      parameterProps

    }.toSet

    val mainClassType = endpointsSet.filter {
      sType => sType.name.toLowerCase.contains(sumoSwaggerClassName.toLowerCase)
    }.head

    val otherTypes = mainClassType.props.filter {
      prop => !prop.getType().props.isEmpty
    }

    Set(mainClassType) ++ otherTypes.map(_.getType()).toSet
  }

  def getUpdateAndCreateRequestBodyType(): List[SumoSwaggerType] = {
    val endpoints = supportedEndpoints.filter { op: SumoSwaggerEndpoint =>
      op.endpointName.equalsIgnoreCase(SumoSwaggerSupportedOperations.UPDATE + sumoSwaggerClassName) ||
        op.endpointName.equalsIgnoreCase(SumoSwaggerSupportedOperations.CREATE + sumoSwaggerClassName) ||
        op.responses.map(_.respTypeName).contains(sumoSwaggerClassName)
    }

    val types = endpoints.flatMap {
      endpoint => endpoint.parameters.filter(_.paramType == SumoTerraformSupportedParameterTypes.BodyParameter)
        .flatMap(_.param.getAllTypes())
    }

    types.filter(_.name.toUpperCase.contains(sumoSwaggerClassName.toUpperCase())).toSet.toList
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
    if (camelOp == "Get") {
      s"$prefix${sumoSwaggerClassName.capitalize}Read"
    } else {
      s"$prefix${sumoSwaggerClassName.capitalize}$camelOp"
    }
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

    typesUsed.find {
      t => t.name.toUpperCase == sumoSwaggerClassName.toUpperCase() || t.name.toUpperCase.contains(sumoSwaggerClassName.toUpperCase)
    }.getOrElse {
      freakOut("WTF??????WTF??????WTF??????WTF?????? => NO MAIN CLASS for " + sumoSwaggerClassName)
      throw new RuntimeException("This should not happen in getMainObjectClass ")
    }
  }

  def getResourceFuncMappings(): String = {
    // TODO Each type used needs to be generated somewhere for this to work, for now...
    // ... hoping that this is all basic types

    val funcMappings: String = getFunctionMappings(crud.filter(_.toLowerCase != "exists")).mkString(",\n      ").concat(",")

    // TODO This assumption is too optimistic
    val classes = getUpdateAndCreateRequestBodyType()

    val classesProps = classes.flatMap(_.props.filter(prop => !prop.getName().toLowerCase.contains("created") &&
      !prop.getName().toLowerCase.contains("modified") && !prop.getName().toLowerCase.contains("system") &&
      !prop.getName().toLowerCase.equals("id"))).toSet //.filter(_.getRequired()).toSet


    val propsObjects = classesProps.map {
      sumoSwaggerObject: SumoSwaggerObject =>
        sumoSwaggerObject.getAsTerraformSchemaType(false)
    }.toList.toSet.mkString(",\n         ").concat(",")

    s"""func resourceSumologic${sumoSwaggerClassName.capitalize}() *schema.Resource {
       |    return &schema.Resource{
       |      $funcMappings
       |      Importer: &schema.ResourceImporter{
       |        State: schema.ImportStatePassthrough,
       |      },
       |
      |       Schema: map[string]*schema.Schema{
       |        $propsObjects
       |    },
       |  }
       |}""".stripMargin
  }

}