package com.sumologic.terraform_generator.utils

import com.sumologic.terraform_generator.TerraformGeneratorHelper
import com.sumologic.terraform_generator.objects.{SumoSwaggerEndpoint, SumoSwaggerObject, SumoSwaggerObjectArray, SumoSwaggerObjectSingle, SumoSwaggerParameter, SumoSwaggerResponse, SumoSwaggerTemplate, SumoSwaggerType}
import com.sumologic.terraform_generator.utils.SumoTerraformPrinter.freakOut
import io.swagger.v3.oas.models.PathItem.HttpMethod
import io.swagger.v3.oas.models.media.{ArraySchema, ComposedSchema, Schema}
import io.swagger.v3.oas.models.parameters.{Parameter, PathParameter, QueryParameter}
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.{OpenAPI, Operation, PathItem}

import scala.collection.JavaConverters._

object SumoTerraformPrinter {
  final val shouldPrint = false

  def freakOut(msg: String): Unit = {
    println("###### Freaking out about [" + msg + "]\n")
  }
}

object SumoTerraformSchemaTypes {
  final val TypeBool = "schema.TypeBool"
  final val TypeInt = "schema.TypeInt"
  final val TypeFloat = "schema.TypeFloat"
  final val TypeString = "schema.TypeString"
  // Date is represented using TypeString
  final val TypeMap = "schema.TypeMap"
  final val TypeList = "schema.TypeList"
  final val TypeSet = "schema.TypeSet"

  def swaggerTypeToTerraformSchemaType(swaggerType: String): String = {
    swaggerType match {
      case "string" => TypeString
      case "int" => TypeInt
      case "bool" => TypeBool
      case "float" => TypeFloat // TODO does float exist in swagger?
      case "array" => TypeList
      case _ => TypeMap // TODO this generalization is way too naiive
      //TODO Date type? translates to TypeString
    }
  }

  def swaggerTypeToGoType(swaggerType: String): String = {
    swaggerType match {
      case "boolean" => "bool"
      case "object" => "map[string]string"
      case "array" => "[]string"
      case _ => swaggerType
    }
  }

  def swaggerTypeToPlaceholder(swaggerType: String): String = {
    swaggerType match {
      case "bool" => "%t"
      case "array" => "%v"
      case _ => "\"%s\""
    }
  }
}

object SumoTerraformSupportedParameterTypes {
  final val PathParameter = "PathParameter"
  final val BodyParameter = "BodyParameter"
  final val QueryParameter = "QueryParameter"
}

object SumoTerraformSupportedPrintfTypes {
  final val StringFormat = "string"
  final val IntFormat = "int"

  def toPrintfType(formatType: String): String = {
    formatType match {
      case StringFormat => "%s"
      case IntFormat => "%d"
      case _ => throw new RuntimeException(s"Unsupported Printf Type ($formatType)")
    }
  }
}

object SumoTerraformUtils extends TerraformGeneratorHelper {
  val packageName = "sumologic"

  def varNameFromTypeName(typeName: String): String = {
    typeName.substring(0, 1).toLowerCase() + typeName.substring(1)
  }

  def httpMethodToTerraformHttpClientFunctionName(httpMethod: String): String = {
    httpMethod.substring(0, 1).toUpperCase() + httpMethod.substring(1).toLowerCase
  }

  def getTaggedComponents(openAPI: OpenAPI): Map[String, Schema[_]] = {
    val componentsWithExtensions = openAPI.getComponents.getSchemas.asScala.toList.filter {
      schema =>
        schema._2.getExtensions != null
    }.toMap

    componentsWithExtensions.map {
      case (name, schema) =>
        if (schema.getExtensions.asScala.contains("x-tf-resource-name")) {
          (schema.getExtensions.asScala.get("x-tf-resource-name").get.toString, schema)
        } else {
          (name, schema)
        }
    }.toMap

  }

  def getComponent(openAPI: OpenAPI, name: String): (String, Schema[_]) = {
    val component = openAPI.getComponents.getSchemas.asScala.filter(_._1 == name).head
    if (component._2.getExtensions != null && component._2.getExtensions.asScala.contains("x-tf-resource-name")) {
      (component._2.getExtensions.asScala.get("x-tf-resource-name").get.toString, component._2)
    } else {
      component
    }
  }

  def getTaggedProperties(openAPI: OpenAPI, model: Schema[_]): List[(String, Schema[_])] = {
    if (model.getExtensions != null) {
      val propNames = model.getExtensions.asScala.get("x-tf-generated-properties").get.toString.split(",").toList
      val refProps = if (model.isInstanceOf[ComposedSchema]) {
        val allOfRefs = model.asInstanceOf[ComposedSchema].getAllOf.asScala.map{
          child =>
            if (child.get$ref() != null) {
              child.get$ref().split("/").last
            } else {
              null
            }
        }.filter(_ != null)
        allOfRefs.map(getComponent(openAPI, _)._2.getProperties.asScala.toList).flatten.toList
      } else {
        List.empty[(String, Schema[_])]
      }

      val allProps = if (model.isInstanceOf[ComposedSchema]) {
        model.asInstanceOf[ComposedSchema].getAllOf.asScala.last.getProperties.asScala.toList ++ refProps
      } else {
        model.getProperties.asScala.toList ++ refProps
      }
      allProps.filter {
        prop => propNames.contains(prop._1)
      }
    } else {
      List.empty[(String, Schema[_])]
    }
  }

  def getRequiredProperties(openAPI: OpenAPI, model: Schema[_]): List[String] = {
    if (model.getExtensions != null) {
      val refRequired = if (model.isInstanceOf[ComposedSchema]) {
        val allOfRefs = model.asInstanceOf[ComposedSchema].getAllOf.asScala.map{
          child =>
            if (child.get$ref() != null) {
              child.get$ref().split("/").last
            } else {
              null
            }
        }.filter(_ != null)
        allOfRefs.map(getComponent(openAPI, _)._2.getRequired.asScala).flatten.toList
      } else {
        List.empty[String]
      }

      val allRequired = if (model.isInstanceOf[ComposedSchema]) {
        model.asInstanceOf[ComposedSchema].getAllOf.asScala.last.getRequired.asScala ++ refRequired
      } else {
        model.getRequired.asScala ++ refRequired
      }
      allRequired.toList
    } else {
      model.getRequired.asScala.toList
    }
  }

  // TODO we need a query param version of this as well
  def pathParamsToPrintfFormat(path: String, pathParamList: List[SumoSwaggerParameter]): (String, String) = {
    if (pathParamList.isEmpty) {
      (path, "")
    } else {
      val (firstList, restOfParamList) = pathParamList.splitAt(1)
      val firstParam = firstList.head.param
      val formattedPath = path.replaceFirst(s"\\{${firstParam.getName}\\}",
        SumoTerraformSupportedPrintfTypes.toPrintfType(firstParam.getType().name))
      val (newPath, newArgs) = pathParamsToPrintfFormat(formattedPath, restOfParamList)
      (newPath, s", ${firstParam.getName}" + newArgs)
    }
  }


  def bodyParamToArgList(bodyParamOption: Option[SumoSwaggerParameter]): List[String] = {
    bodyParamOption match {
      case Some(bodyParam) =>
        val requestBodyType = bodyParam.param.getType().name
        List[String](varNameFromTypeName(requestBodyType) + " " + requestBodyType)
      case None => List[String]()
    }
  }

  def paramListToArgList(params: List[SumoSwaggerParameter]): List[String] = {
    params.map(_.toTerraformFuncArg())
  }

  def makeArgsListForDecl(params: List[SumoSwaggerParameter]): String = {
    val allParams = getArgsListForDecl(params)
    allParams.mkString(", ")
  }

  def getArgsListForDecl(params: List[SumoSwaggerParameter]): List[String] = {
    // TODO val queryParamList = params.filter(_.paramType == SumoTerraformSupportedParameterTypes.QueryParameter)
    // TODO Need to consider required params
    val pathParamList = params.filter(
      _.paramType == SumoTerraformSupportedParameterTypes.PathParameter)

    val bodyParamOpt = params.find(_.paramType == SumoTerraformSupportedParameterTypes.BodyParameter)

    // paramListToArgList(pathParamList) ++ bodyParamToArgList(bodyParamOpt)

    if (bodyParamOpt.isDefined) {
      bodyParamToArgList(bodyParamOpt)
    } else {
      paramListToArgList(pathParamList)
    }
  }

  def getArgsListAsVariableAssignmentsFromDataSourceSchema(params: List[SumoSwaggerParameter]): List[String] = {
    // TODO add QueryParams too
    val pathParamList = params.filter(_.paramType == SumoTerraformSupportedParameterTypes.PathParameter)
    val bodyParamList = params.filter(_.paramType == SumoTerraformSupportedParameterTypes.BodyParameter)
    val allParams = pathParamList ++ bodyParamList
    allParams.map {
      param: SumoSwaggerParameter =>
        if (param.param.getType().isCompositeType()) {
          val callSite = getTerraformResourceDataToObjectConverterFuncCall(param.param.getType())
          val varName = varNameFromTypeName(param.param.getType().name)
          s"$varName = $callSite"
        } else {
          getTerraformDataSourceValueSetCheck(param.param.getName(), singleReturn = true)
        }
    }
  }

  def resolvePropertyType(openApi: OpenAPI, property: Schema[_]): SumoSwaggerType = {
    if (property.get$ref() != null) {
      val model = getComponent(openApi, property.get$ref().split("/").last)._2
      processModel(openApi, property.get$ref(), model)
    } else {
      SumoSwaggerType(SumoTerraformSchemaTypes.swaggerTypeToGoType(property.getType))
    }
  }

  def makeTerraformUrlFormatForSprintf(path: String, params: List[SumoSwaggerParameter]): String = {
    // TODO val queryParamList = params.filter(_.paramType == SumoTerraformSupportedParameterTypes.QueryParameter)
    // TODO Need to consider required params

    val pathParamList = params.filter(
      _.paramType == SumoTerraformSupportedParameterTypes.PathParameter)
    val (printfFormattedPath, printfArgs) = SumoTerraformUtils.pathParamsToPrintfFormat(
      path, pathParamList)

    s""""$printfFormattedPath"$printfArgs""".stripMargin
  }

  // TODO IMPORTANT TO DO LATER IS THAT WHEN REFERENCE OBJECTS ARE CALCULATED, WE SHOULD PUT THEM IN A MAP
  // TO BE USED AS A CACHE, BECAUSE THESE GET RECALCULATED WHEN RESPONSE OBJECTS ARE IDENTICAL AS IN GET/LIST
  def processResponseObjects(openApi: OpenAPI, responses: Map[String, ApiResponse]): List[SumoSwaggerResponse] = {
    responses.flatMap {
      case (respName: String, respObj: ApiResponse) =>
        if (respObj.getContent != null && respObj.getContent.get("application/json") != null && respObj.getContent.get("application/json").getSchema != null) {
          Option(respObj.getContent.get("application/json").getSchema.get$ref()) match {
            case Some(ref) =>
              val resourceName = ref.split("/").toList.last
              if (getTaggedComponents(openApi).get(resourceName).isDefined) {
                val swaggerType = processModel(openApi, ref, getTaggedComponents(openApi).get(resourceName).get)
                List(SumoSwaggerResponse(respName, Some(swaggerType)))
              } else {
                val responseTypes = getTaggedComponents(openApi).filter {
                  component =>
                    if (component.isInstanceOf[ComposedSchema]) {
                      (component._2.asInstanceOf[ComposedSchema].getAllOf != null && component._2.asInstanceOf[ComposedSchema].getAllOf.asScala.count {
                        x => x.get$ref() != null && x.get$ref().toLowerCase.contains(resourceName.toLowerCase)
                      } == 1)
                    } else {
                      component._1.toLowerCase.contains(resourceName.toLowerCase)
                    }
                }
                if (responseTypes.size >= 1) {
                  responseTypes.toList.map {
                    case (name, schema) =>
                      val swaggerType = processModel(openApi, name, schema)
                      SumoSwaggerResponse(name, Some(swaggerType))
                  }
                } else {
                  List(SumoSwaggerResponse(respName, None))
                }
              }
            case _ =>
              freakOut(s"*** Response  => $respName WTF?????? Response Schema Type ")
              throw new RuntimeException("This should not happen in processResponseObjects ")
          }
        } else {
          // No response body
          List(SumoSwaggerResponse(respName, None))
        }
    }.toList
  }

  def processPathParameter(openApi: OpenAPI, pathParam: PathParameter): SumoSwaggerParameter = {
    // PathParameter is of type SerializableParameter
    SumoSwaggerParameter(SumoTerraformSupportedParameterTypes.PathParameter,
      SumoSwaggerObjectSingle(pathParam.getName,
        SumoSwaggerType(pathParam.getSchema.getType, List[SumoSwaggerObject]()),
        pathParam.getRequired, Some(pathParam.getSchema.getDefault.asInstanceOf[AnyRef]), pathParam.getSchema.getDescription, ""))
  }

  def processQueryParameter(openApi: OpenAPI, queryParam: QueryParameter): SumoSwaggerParameter = {
    // QueryParameter is of type SerializableParameter
    SumoSwaggerParameter(SumoTerraformSupportedParameterTypes.QueryParameter,
      SumoSwaggerObjectSingle(queryParam.getName,
        SumoSwaggerType(queryParam.getSchema.getType, List[SumoSwaggerObject]()),
        queryParam.getRequired, Some(queryParam.getSchema.getDefault.asInstanceOf[AnyRef]), queryParam.getSchema.getDescription, ""))
  }

  def processBodyParameter(openApi: OpenAPI, bodyParam: Schema[_], baseType: String): List[SumoSwaggerParameter] = {
    // We should just use the response object for this

    val defName: String = bodyParam.get$ref().split("#/components/schemas/").last
    val modelOpt = Option(getComponent(openApi, defName)._2)
    val taggedResource = getTaggedComponents(openApi).filter {
      component =>
        if (component.isInstanceOf[ComposedSchema]) {
          component._1.toLowerCase.contains(baseType.toLowerCase) || (component._2.asInstanceOf[ComposedSchema].getAllOf != null && component._2.asInstanceOf[ComposedSchema].getAllOf.asScala.count {
            x => x.get$ref() != null && x.get$ref().toLowerCase.contains(baseType.toLowerCase)
          } == 1)
        } else {
          component._1.toLowerCase.contains(baseType.toLowerCase)
        }
    }

    taggedResource.map {
      resource =>
        val modelName = if (resource._1.contains("/")) {
          resource._1.split("/").last
        } else {
          resource._1
        }

        modelOpt match {
          case Some(model) =>
            //maybePrint(s" #### DEF ($defName)  => ${model.getProperties}")
            val swaggerType = processModel(openApi, modelName, resource._2)
            //swaggerType.terraformify()
            SumoSwaggerParameter(SumoTerraformSupportedParameterTypes.BodyParameter,
              SumoSwaggerObjectSingle(modelName, swaggerType, true, None, resource._2.getDescription, ""))
          case None =>
            freakOut("processBodyParameter " + defName)
            throw new RuntimeException("This should not happen in processBodyParameter ")
        }
    }.toList
  }

  def processComposedModel(openApi: OpenAPI, modelDefName: String, composedModel: ComposedSchema): SumoSwaggerType = {
    val parts: List[SumoSwaggerObject] = composedModel.getAllOf.asScala.flatMap {
      case model: Schema[_] =>
        if (model.get$ref() != null) {
          processModel(openApi, model.get$ref(), model).props
        } else {
          if (model.getProperties != null) {
            val taggedProps = getTaggedProperties(openApi, composedModel)
            taggedProps.map {
              propTuple =>
                processModelProperty(openApi, propTuple._1, propTuple._2, taggedProps.map(_._1).intersect(getRequiredProperties(openApi, composedModel)))
            }
          } else {
            List[SumoSwaggerObject]()
          }
        }
    }.toList

    val partsWithId = if (!parts.map(_.getName().toLowerCase).contains("id") || parts.flatMap(_.getAllTypes().map(_.name.toLowerCase)).contains("id")) {
      parts ++ List(SumoSwaggerObjectSingle("id", SumoSwaggerType("string"), false, None, "", ""))
    } else {
      parts
    }
    if (modelDefName.contains("Model")) {
      SumoSwaggerType(modelDefName.replace("Model", ""), partsWithId.toSet.toList)
    } else {
      SumoSwaggerType(modelDefName, partsWithId.toSet.toList)
    }
  }

  def processModel(openApi: OpenAPI, modelDefName: String, model: Schema[_]): SumoSwaggerType = {
    val modelName = if (modelDefName.contains("/")) {
      modelDefName.split("/").last
    } else {
      modelDefName
    }

    model match {
      case arrayModel: ArraySchema =>
        //freakOut("arrayModel => " + arrayModel)
        SumoSwaggerType(modelName, List[SumoSwaggerObject]())
      case composedModel: ComposedSchema =>
        processComposedModel(openApi, modelName, composedModel)
      case _ =>
        if (model.get$ref() != null) {
          //freakOut("REF MODEL FOR " + refModel + " IS " + refModel.getSimpleRef + " props " + refModel.getProperties)
          val resourceName = model.get$ref().split("/").toList.last
          if (getTaggedComponents(openApi).get(resourceName).isDefined) {
            val resolvedRefModel = getTaggedComponents(openApi).get(resourceName).get
            processModel(openApi, modelName, resolvedRefModel)
          } else {
            processModel(openApi, modelName, getComponent(openApi, resourceName)._2)
          }
        } else if (model.get$ref() == null) {
          val props: List[SumoSwaggerObject] = if (model.getExtensions != null) {
            getTaggedProperties(openApi, model).map {
              propTuple =>
                val taggedProps = getTaggedProperties(openApi, model)
                // processModelProperty(openApi, propTuple._1, propTuple._2, propTuple._2.getRequired.asScala.toList)
                processModelProperty(openApi, propTuple._1, propTuple._2, taggedProps.map(_._1).intersect(getRequiredProperties(openApi, model)))
            }
          } else {
            if (model.getProperties != null && modelDefName.contains("/")) {
              model.getProperties.asScala.toList.map {
                prop => processModelProperty(openApi, prop._1, prop._2, List(prop._1).intersect(getRequiredProperties(openApi, model)))
              }
            } else {
              List[SumoSwaggerObject]()
            }
          }

          val propsWithId = if (!props.map(_.getName().toLowerCase).contains("id")) {
            props ++ List(SumoSwaggerObjectSingle("id", SumoSwaggerType("string"), false, None, "", ""))
          } else {
            props
          }
          // literally only because of RoleModel
          if (modelName.contains("Model")) {
            SumoSwaggerType(modelName.replace("Model", ""), propsWithId.toSet.toList)
          } else {
            SumoSwaggerType(modelName, propsWithId.toSet.toList)
          }
        } else {
          //freakOut("DONT KNOW WHAT THIS IS " + modelDefName)
          if (modelName.contains("Model")) {
            SumoSwaggerType(modelName.replace("Model", ""), List[SumoSwaggerObject]())
          } else {
            SumoSwaggerType(modelName, List[SumoSwaggerObject]())
          }
        }
    }
  }

  def processModelProperty(openApi: OpenAPI, propName: String, prop: Schema[_], requiredProps: List[String]): SumoSwaggerObject = {

    val example = if (prop.getExample == null) {""} else {prop.getExample.toString}
    if (prop.isInstanceOf[ArraySchema]) {
      val arrayProp = prop.asInstanceOf[ArraySchema]
      SumoSwaggerObjectArray(propName, resolvePropertyType(openApi, arrayProp.getItems), requiredProps.contains(arrayProp.getName), None, prop.getDescription, example)
    } else {
      if (prop.get$ref() != null) {
        val refModel = getComponent(openApi, prop.get$ref().split("/").last)._2
        SumoSwaggerObjectSingle(propName, processModel(openApi, propName, refModel), requiredProps.contains(prop.getName), None, prop.getDescription, example)
      } else {
        if (propName.toLowerCase != "id") {
          SumoSwaggerObjectSingle(propName, resolvePropertyType(openApi, prop),requiredProps.map(_.toLowerCase).contains(propName.toLowerCase), None, prop.getDescription, example)
        } else {
          SumoSwaggerObjectSingle(propName, resolvePropertyType(openApi, prop),requiredProps.map(_.toLowerCase).contains(propName.toLowerCase), None, prop.getDescription, "")
        }
      }
    }
  }

  def processOperation(openApi: OpenAPI, operation: Operation, pathName: String, method: HttpMethod, baseType: String): SumoSwaggerEndpoint = {
    val responses = processResponseObjects(openApi, operation.getResponses.asScala.toMap)

    val params: List[SumoSwaggerParameter] = operation.getParameters.asScala.flatMap { param: Parameter =>
      param match {
        case pathParam: PathParameter =>
          List(processPathParameter(openApi, pathParam))
        case queryParam: QueryParameter =>
          List(processQueryParameter(openApi, queryParam))
        case _ =>
          if (param.getIn == null && param.getContent != null) {
            processBodyParameter(openApi, param.getSchema, baseType)
          }
          freakOut("WTF??????WTF??????WTF??????WTF?????? => " + param)
          throw new RuntimeException("This should not happen in processOperation ")
      }
    }.toList

    val requestBody: List[SumoSwaggerParameter] = if (operation.getRequestBody != null) {
      processBodyParameter(openApi, operation.getRequestBody.getContent.get("application/json").getSchema, baseType)
    } else {
      List.empty[SumoSwaggerParameter]
    }

    val allParams = params ++ requestBody
    SumoSwaggerEndpoint(operation.getOperationId, pathName, method.name(), allParams, responses)
  }

  def processPath(openApi: OpenAPI, path: PathItem, pathName: String, baseType: String):
  List[SumoSwaggerEndpoint] = {
    val operationMap = Map(
      HttpMethod.GET -> path.getGet,
      HttpMethod.POST -> path.getPost,
      HttpMethod.DELETE -> path.getDelete,
      HttpMethod.PUT -> path.getPut
    )
    val filteredOps = operationMap.filter {
      methodOperationTuple =>
        methodOperationTuple._2 != null && methodOperationTuple._2.getExtensions != null
    }

    filteredOps.map {
      case (method: HttpMethod, operation: Operation) =>
        processOperation(openApi, operation, pathName, method, baseType)
    }.toList
  }

  def processClass(openApi: OpenAPI, baseType: String): SumoSwaggerTemplate = {
    val filteredPaths = openApi.getPaths.asScala.filter {
      case (pathName: String, path: PathItem) =>
        val postExtensions = if (path.getPost != null && path.getPost.getExtensions != null) {
          path.getPost.getExtensions.asScala.filterKeys(_ == "x-tf-create")
        } else List.empty[(String, AnyRef)]
        val getExtensions = if (path.getGet != null && path.getGet.getExtensions != null) {
          path.getGet.getExtensions.asScala.filterKeys(_ == "x-tf-read")
        } else List.empty[(String, AnyRef)]
        val putExtensions = if (path.getPut != null && path.getPut.getExtensions != null) {
          path.getPut.getExtensions.asScala.filterKeys(_ == "x-tf-update")
        } else List.empty[(String, AnyRef)]
        val deleteExtensions = if (path.getDelete != null && path.getDelete.getExtensions != null) {
          path.getDelete.getExtensions.asScala.filterKeys(_ == "x-tf-delete")
        } else List.empty[(String, AnyRef)]
        val vendorExtensions = postExtensions ++ getExtensions ++ putExtensions ++ deleteExtensions

        (!vendorExtensions.isEmpty) &&
          (pathName.toLowerCase.contains(baseType.toLowerCase + "s") ||
            pathName.replaceAll("Model", "").toLowerCase.contains(baseType.toLowerCase + "s"))
    }



    val endpoints = filteredPaths.flatMap {
      case (pathName: String, path: PathItem) => processPath(openApi, path, pathName, baseType)
    }.toList

    SumoSwaggerTemplate(baseType, endpoints)
  }

  def processAllClasses(openApi: OpenAPI,
                        types: List[String] = List.empty[String]): List[(SumoSwaggerTemplate, String)] = {
    val filteredPaths = openApi.getPaths.asScala.filter {
      case (pathName: String, path: PathItem) =>
        val postExtensions = if (path.getPost != null && path.getPost.getExtensions != null) {
          path.getPost.getExtensions.asScala.filterKeys(_ == "x-tf-create")
        } else List.empty[(String, AnyRef)]
        val getExtensions = if (path.getGet != null && path.getGet.getExtensions != null) {
          path.getGet.getExtensions.asScala.filterKeys(_ == "x-tf-read")
        } else List.empty[(String, AnyRef)]
        val putExtensions = if (path.getPut != null && path.getPut.getExtensions != null) {
          path.getPut.getExtensions.asScala.filterKeys(_ == "x-tf-update")
        } else List.empty[(String, AnyRef)]
        val deleteExtensions = if (path.getDelete != null && path.getDelete.getExtensions != null) {
          path.getDelete.getExtensions.asScala.filterKeys(_ == "x-tf-delete")
        } else List.empty[(String, AnyRef)]
        val vendorExtensions = postExtensions ++ getExtensions ++ putExtensions ++ deleteExtensions

        !vendorExtensions.isEmpty
    }.map {
      case (pathName: String, path: PathItem) =>
        if (path.getPost != null) {
          if (path.getPost.getTags != null) {
            (pathName, path, path.getPost.getTags.asScala.head)
          } else {
            (pathName, path, "noTag")
          }
        } else if (path.getGet != null) {
          if (path.getGet.getTags != null) {
            (pathName, path, path.getGet.getTags.asScala.head)
          } else {
            (pathName, path, "noTag")
          }
        } else if (path.getPut != null) {
          if (path.getPut.getTags != null) {
            (pathName, path, path.getPut.getTags.asScala.head)
          } else {
            (pathName, path, "noTag")
          }
        } else if (path.getDelete != null) {
          if (path.getDelete.getTags != null) {
            (pathName, path, path.getDelete.getTags.asScala.head)
          } else {
            (pathName, path, "noTag")
          }
        } else {
          (pathName, path, "noTag")
        }
    }

    val templates = filteredPaths.groupBy(_._3).flatMap {
      case (tag: String, paths: Iterable[(String, PathItem, String)]) =>
        val baseTypeName = tag.replace("Management", "")
        val endpoints = paths.flatMap {
          path: (String, PathItem, String) => processPath(openApi, path._2, path._1, baseTypeName)
        }
        val baseTypes = endpoints.flatMap {
          endpoint =>
            endpoint.responses.map {
              response =>
                if (response.respTypeOpt.isDefined) {
                  response.respTypeOpt.get.name
                } else {
                  null
                }
            }
        }.toSet.filter(_ != null)
        baseTypes.map {
          baseType =>
            (SumoSwaggerTemplate(baseType, endpoints.toList), baseType)
        }
    }.toList

    if (types.nonEmpty) {
      templates.filter {
        template => types.exists {
          t => template._2.toLowerCase.contains(t.toLowerCase)
        }
      }
    } else {
      templates
    }
  }

  def seperatorLine(title: String = ""): String = {
    s"\n\n// -----------------------------$title-----------------------------//\n\n"
  }

  def getTerraformResourceSetters(propName: String, objName: String): String = {
    val noCamelCaseName = removeCamelCase(propName)
    s"""resourceData.Set("${noCamelCaseName.toLowerCase}", $objName.${propName.capitalize})""".stripMargin
  }

  def getTerraformResourceGetters(prop: SumoSwaggerObject): String = {
    val propName = prop.getName()
    val noCamelCaseName = removeCamelCase(propName)
    val propType = prop.getType().name
    prop match {
      case arrayProp: SumoSwaggerObjectArray =>
        s"""${propName.capitalize}: ${propName.toLowerCase},""".stripMargin
      case singleProp: SumoSwaggerObject =>
        if (singleProp.getName().toLowerCase == "id") {
          s"""${propName.toUpperCase}: d.Id(),""".stripMargin
        } else {
          s"""${propName.capitalize}: d.Get(\"${noCamelCaseName.toLowerCase}\").($propType),""".stripMargin
        }
    }
  }

  def getTerraformResourceValidators(objClass: SumoSwaggerType): String = {
    objClass.props.map {
      prop: SumoSwaggerObject =>
        getTerraformResourceValidator(prop, objClass.name)
    }.mkString("")
  }

  def getTerraformDataSourceValueSetCheck(varName: String,
                                          varTypeOpt: Option[String] = None,
                                          singleReturn: Boolean = false): String = {
    val singleReturnTxt = singleReturn match {
      case false => "nil, "
      case true => ""
    }
    val typeInfo = varTypeOpt match {
      case Some(varType) => s" to construct $varType"
      case None => ""
    }
    val noCamelCaseName = removeCamelCase(varName)
    if (varName.toLowerCase.contains("id")) {
      /* s"""${varName}Exists, ok := resourceData.Id();
         |if !ok {
         |    return ${singleReturnTxt}fmt.Errorf("SumologicTerraformError: %q is required${typeInfo}.", ${varName}Exists)
         |  }\n""".stripMargin */
      ""
    } else {
      s"""${varName}Exists, ok := resourceData.GetOkExists("$noCamelCaseName");
         |if !ok {
         |    return ${singleReturnTxt}fmt.Errorf("SumologicTerraformError: %q is required${typeInfo}.", ${varName}Exists)
         |  }\n""".stripMargin
    }
  }

  def getTerraformResourceValidator(prop: SumoSwaggerObject, typeName: String): String = {
    prop.getRequired() match {
      case true => getTerraformDataSourceValueSetCheck(prop.getName(), Option(prop.getType().name))
      case false => ""
    }
  }

  def getTerraformObjectToResourceDataConverterFuncCall(objClass: SumoSwaggerType): String = {
    val objName = varNameFromTypeName(objClass.name)
    s"${objName}ToResourceData(resourceData, $objName)"
  }

  def getTerraformObjectToResourceDataConverter(objClass: SumoSwaggerType): String = {
    val className = objClass.name
    val objName = varNameFromTypeName(objClass.name)

    val setters = objClass.props.map {
      prop: SumoSwaggerObject =>
        getTerraformResourceSetters(prop.getName(), objName)
    }.mkString("\n")

    s"""func ${objName}ToResourceData(resourceData *schema.ResourceData, $objName *$className) {
       |   $setters
       | }""".stripMargin

  }

  def getTerraformResourceDataToObjectConverterFuncName(objClass: SumoSwaggerType): String = {
    s"resourceTo${objClass.name}"
  }

  def getTerraformResourceDataToObjectConverterFuncCall(objClass: SumoSwaggerType): String = {
    val funcName = getTerraformResourceDataToObjectConverterFuncName(objClass)
    s"$funcName(resourceData)"
  }

  def getTerraformResourceDataToObjectConverter(objClass: SumoSwaggerType, skipValidators: Boolean): String = {
    val className = objClass.name

    val getters = objClass.props.map {
      prop: SumoSwaggerObject =>
        getTerraformResourceGetters(prop)
    }.mkString("\n    ")

    val funcName = getTerraformResourceDataToObjectConverterFuncName(objClass)

    val arrayBlock = objClass.props.filter {
      prop => prop.isInstanceOf[SumoSwaggerObjectArray]
    }.map {
      prop => s"""raw${prop.getName().capitalize} := d.Get("${prop.getName().toLowerCase}").([]interface{})
                 |	${prop.getName().toLowerCase} := make([]string, len(raw${prop.getName().capitalize} ))
                 |	for i, v := range raw${prop.getName().capitalize}  {
                 |		${prop.getName().toLowerCase}[i] = v.(string)
                 |	}""".stripMargin
    }.mkString("\n")
    s"""func $funcName(d *schema.ResourceData) $className {
       |   $arrayBlock
       |   return $className{
       |    $getters
       |   }
       | }""".stripMargin
  }
}