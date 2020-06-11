package com.sumologic.terraform_generator.utils

import com.sumologic.terraform_generator.objects.{ScalaSwaggerEndpoint, ScalaSwaggerObject, ScalaSwaggerObjectArray, ScalaSwaggerObjectSingle, ScalaSwaggerParameter, ScalaSwaggerResponse, ScalaSwaggerTemplate, ScalaSwaggerType, TerraformSchemaTypes, TerraformSupportedParameterTypes}
import io.swagger.v3.oas.models.PathItem.HttpMethod
import io.swagger.v3.oas.models.media.{ArraySchema, ComposedSchema, Schema}
import io.swagger.v3.oas.models.parameters.{HeaderParameter, Parameter, PathParameter, QueryParameter}
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.{OpenAPI, Operation, PathItem}

import scala.collection.JavaConverters._

object OpenApiProcessor extends ProcessorHelper {

  def resolvePropertyType(openApi: OpenAPI, property: Schema[_]): ScalaSwaggerType = {
    if (property.get$ref() != null) {
      val model = getComponent(openApi, property.get$ref().split("/").last)._2
      processModel(openApi, property.get$ref(), model)
    } else {
      ScalaSwaggerType(TerraformSchemaTypes.swaggerTypeToGoType(property.getType))

    }
  }

  // TODO IMPORTANT TO DO LATER IS THAT WHEN REFERENCE OBJECTS ARE CALCULATED, WE SHOULD PUT THEM IN A MAP
  // TO BE USED AS A CACHE, BECAUSE THESE GET RECALCULATED WHEN RESPONSE OBJECTS ARE IDENTICAL AS IN GET/LIST
  def processResponseObjects(openApi: OpenAPI, responses: Map[String, ApiResponse]): List[ScalaSwaggerResponse] = {
    responses.flatMap {
      case (respName: String, respObj: ApiResponse) =>
        if (respObj.getContent != null && respObj.getContent.get("application/json") != null && respObj.getContent.get("application/json").getSchema != null) {
          Option(respObj.getContent.get("application/json").getSchema.get$ref()) match {
            case Some(ref) =>
              val resourceName = ref.split("/").toList.last.replace("Model", "").replace("BaseDefinitionUpdate", "").replace("BaseResponse", "")
              if (getTaggedComponents(openApi).get(resourceName).isDefined) {
                val swaggerType = processModel(openApi, ref, getTaggedComponents(openApi).get(resourceName).get)
                List(ScalaSwaggerResponse(respName, Some(swaggerType)))
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
                      ScalaSwaggerResponse(name, Some(swaggerType))
                  }
                } else {
                  List(ScalaSwaggerResponse(respName, None))
                }
              }
            case _ =>
              throw new RuntimeException("This should not happen in processResponseObjects ")
          }
        } else {
          // No response body
          List(ScalaSwaggerResponse(respName, None))
        }
    }.toList
  }

  def processPathParameter(openApi: OpenAPI, pathParam: PathParameter): ScalaSwaggerParameter = {
    ScalaSwaggerParameter(TerraformSupportedParameterTypes.PathParameter,
      ScalaSwaggerObjectSingle(pathParam.getName,
        ScalaSwaggerType(pathParam.getSchema.getType, List[ScalaSwaggerObject]()),
        pathParam.getRequired, Some(pathParam.getSchema.getDefault.asInstanceOf[AnyRef]), pathParam.getSchema.getDescription, ""))
  }

  def processQueryParameter(openApi: OpenAPI, queryParam: QueryParameter): ScalaSwaggerParameter = {
    ScalaSwaggerParameter(TerraformSupportedParameterTypes.QueryParameter,
      ScalaSwaggerObjectSingle(
        queryParam.getName,
        ScalaSwaggerType(
          queryParam.getSchema.getType,
          List[ScalaSwaggerObject]()),
        queryParam.getRequired,
        Some(queryParam.getSchema.getDefault.asInstanceOf[AnyRef]),
        queryParam.getSchema.getDescription,
        Option(queryParam.getExample).map {
          example => queryParam.getSchema match {
            case _: ArraySchema =>
              example.asInstanceOf[Array[_]].mkString(", ")
            case _ =>
              example.toString
          }
        }.getOrElse(""),
        Option(queryParam.getSchema).map(_.getPattern).getOrElse("")))
  }

  def processHeaderParameter(openApi: OpenAPI, headerParam: HeaderParameter): ScalaSwaggerParameter = {
    ScalaSwaggerParameter(
      TerraformSupportedParameterTypes.HeaderParameter,
      ScalaSwaggerObjectSingle(
        headerParam.getName,
        ScalaSwaggerType(headerParam.getSchema.getType, List[ScalaSwaggerObject]()),
        headerParam.getRequired,
        Some(headerParam.getSchema.getDefault.asInstanceOf[AnyRef]),
        headerParam.getSchema.getDescription
      )
    )
  }

  def processBodyParameter(openApi: OpenAPI, bodyParam: Schema[_]): List[ScalaSwaggerParameter] = {
    val defName: String = bodyParam.get$ref().split("#/components/schemas/").last
    val (modelName, model) = getComponent(openApi, defName)

    val swaggerType = processModel(openApi, modelName, model)
    val swaggerParameter = ScalaSwaggerParameter(TerraformSupportedParameterTypes.BodyParameter,
      ScalaSwaggerObjectSingle(
        defName,
        swaggerType,
        true,
        None,
        model.getDescription,
        Option(model.getExample).map(_.toString).getOrElse(""),
        Option(model.getPattern).getOrElse("")))

    List(swaggerParameter)
  }

  def processComposedModel(openApi: OpenAPI, modelDefName: String, composedModel: ComposedSchema): ScalaSwaggerType = {
    val parts: List[ScalaSwaggerObject] = composedModel.getAllOf.asScala.flatMap {
      case model: Schema[_] =>
        if (model.get$ref() != null) {
          processModel(openApi, model.get$ref(), model).props
        } else {
          if (model.getProperties != null) {
            val taggedProps = getTaggedProperties(openApi, composedModel)
            taggedProps.map {
              propTuple =>
                processModelProperty(openApi, propTuple._1, propTuple._2, taggedProps.map(_._1).intersect(getRequiredProperties(openApi, composedModel)), modelDefName)
            }
          } else {
            List[ScalaSwaggerObject]()
          }
        }
    }.toList

    val partsWithId = if (!parts.map(_.getName().toLowerCase).contains("id") || parts.flatMap(_.getAllTypes().map(_.name.toLowerCase)).contains("id")) {
      parts ++ List(ScalaSwaggerObjectSingle("id", ScalaSwaggerType("string"), false, None, ""))
    } else {
      parts
    }
    if (modelDefName.contains("Model")) {
      ScalaSwaggerType(modelDefName.replace("Model", ""), partsWithId.toSet.toList)
    } else {
      ScalaSwaggerType(modelDefName, partsWithId.toSet.toList)
    }
  }

  def processModel(openApi: OpenAPI, modelDefName: String, model: Schema[_]): ScalaSwaggerType = {
    val modelName = if (modelDefName.contains("/")) {
      modelDefName.split("/").last
    } else {
      modelDefName
    }

    model match {
      case arrayModel: ArraySchema =>
        ScalaSwaggerType(modelName, List[ScalaSwaggerObject]())
      case composedModel: ComposedSchema =>
        processComposedModel(openApi, modelName, composedModel)
      case _ =>
        if (model.get$ref() != null) {
          val resourceName = model.get$ref().split("/").toList.last
          if (getTaggedComponents(openApi).get(resourceName).isDefined) {
            val resolvedRefModel = getTaggedComponents(openApi).get(resourceName).get
            processModel(openApi, modelName, resolvedRefModel)
          } else {
            processModel(openApi, modelName, getComponent(openApi, resourceName)._2)
          }
        } else if (model.get$ref() == null) {
          val props: List[ScalaSwaggerObject] = if (model.getExtensions != null) {
            getTaggedProperties(openApi, model).map {
              propTuple =>
                val taggedProps = getTaggedProperties(openApi, model)
                processModelProperty(openApi, propTuple._1, propTuple._2, taggedProps.map(_._1).intersect(getRequiredProperties(openApi, model)), modelName)
            }
          } else {
            if (model.getProperties != null && modelDefName.contains("/")) {
              model.getProperties.asScala.toList.map {
                prop => processModelProperty(openApi, prop._1, prop._2, List(prop._1).intersect(getRequiredProperties(openApi, model)), modelName)
              }
            } else {
              List[ScalaSwaggerObject]()
            }
          }

          val propsWithId = if (!props.map(_.getName().toLowerCase).contains("id")) {
            props ++ List(ScalaSwaggerObjectSingle("id", ScalaSwaggerType("string"), false, None, ""))
          } else {
            props
          }
          // literally only because of RoleModel
          if (modelName.contains("Model")) {
            ScalaSwaggerType(modelName.replace("Model", ""), propsWithId.toSet.toList)
          } else {
            ScalaSwaggerType(modelName, propsWithId.toSet.toList)
          }
        } else {
          if (modelName.contains("Model")) {
            ScalaSwaggerType(modelName.replace("Model", ""), List[ScalaSwaggerObject]())
          } else {
            ScalaSwaggerType(modelName, List[ScalaSwaggerObject]())
          }
        }
    }
  }

  def processModelProperty(openApi: OpenAPI, propName: String, prop: Schema[_], requiredProps: List[String], modelName: String): ScalaSwaggerObject = {
    val isWriteOnly = isPropertyWriteOnly(openApi, propName, modelName)
    val format = if (prop.getFormat == null) {""} else {prop.getFormat}
    val example = if (prop.getExample == null) {""} else {prop.getExample.toString}
    val pattern = if (prop.getPattern == null) {""} else {prop.getPattern}
    if (prop.isInstanceOf[ArraySchema]) {
      val arrayProp = prop.asInstanceOf[ArraySchema]
      val itemPattern = if (arrayProp.getItems.getPattern == null) {""} else {arrayProp.getItems.getPattern}
      ScalaSwaggerObjectArray(
        propName,
        resolvePropertyType(openApi, arrayProp),
        requiredProps.contains(arrayProp.getName),
        None,
        prop.getDescription,
        example,
        itemPattern,
        format,
        isWriteOnly)
    } else {
      if (prop.get$ref() != null) {
        val refModel = getComponent(openApi, prop.get$ref().split("/").last)._2
        ScalaSwaggerObjectSingle(
          propName,
          processModel(openApi, propName, refModel),
          requiredProps.contains(prop.getName),
          None,
          prop.getDescription,
          example,
          pattern,
          format,
          isWriteOnly)
      } else {
        if (propName.toLowerCase != "id") {
          ScalaSwaggerObjectSingle(
            propName,
            resolvePropertyType(openApi, prop),
            requiredProps.map(_.toLowerCase).contains(propName.toLowerCase),
            Option(prop.getDefault.asInstanceOf[AnyRef]),
            prop.getDescription,
            example,
            pattern,
            format,
            isWriteOnly)
        } else {
          ScalaSwaggerObjectSingle(
            propName,
            resolvePropertyType(openApi, prop),
            requiredProps.map(_.toLowerCase).contains(propName.toLowerCase),
            Option(prop.getDefault.asInstanceOf[AnyRef]),
            prop.getDescription,
            example,
            pattern,
            format,
            isWriteOnly)
        }
      }
    }
  }

  def processOperation(openApi: OpenAPI, operation: Operation, pathName: String, method: HttpMethod): ScalaSwaggerEndpoint = {
    val responses = processResponseObjects(openApi, operation.getResponses.asScala.toMap)

    val params: List[ScalaSwaggerParameter] = if (operation.getParameters != null) {
      operation.getParameters.asScala.flatMap { param: Parameter =>
        param match {
          case pathParam: PathParameter =>
            List(processPathParameter(openApi, pathParam))
          case queryParam: QueryParameter =>
            List(processQueryParameter(openApi, queryParam))
          case headerParam: HeaderParameter =>
            List(processHeaderParameter(openApi, headerParam))
          case _ =>
            if (param.getIn == null && param.getContent != null) {
              processBodyParameter(openApi, param.getSchema)
            }
            throw new RuntimeException("This should not happen in processOperation ")
        }
      }.toList
    } else {
      List.empty[ScalaSwaggerParameter]
    }

    val requestBody: List[ScalaSwaggerParameter] = if (operation.getRequestBody != null) {
      processBodyParameter(openApi, operation.getRequestBody.getContent.get("application/json").getSchema)
    } else {
      List.empty[ScalaSwaggerParameter]
    }

    val allParams = params ++ requestBody
    val endpointName = operation.getExtensions.asScala.head._2.toString
    ScalaSwaggerEndpoint(endpointName, pathName, method.name(), allParams, responses)
  }

  def processPath(openApi: OpenAPI, path: PathItem, pathName: String):
  List[ScalaSwaggerEndpoint] = {
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
        processOperation(openApi, operation, pathName, method)
    }.toList
  }

  def processAllClasses(openApi: OpenAPI): List[(ScalaSwaggerTemplate, String)] = {
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
            val displayName = openApi.getTags.asScala.filter {
              tag => tag.getName == path.getPost.getTags.asScala.head
            }.map(_.getExtensions.get("x-displayName")).head
            (pathName, path, displayName)
          } else {
            (pathName, path, "noTag")
          }
        } else if (path.getGet != null) {
          if (path.getGet.getTags != null) {
            val displayName = openApi.getTags.asScala.filter {
              tag => tag.getName == path.getGet.getTags.asScala.head
            }.map(_.getExtensions.get("x-displayName")).head
            (pathName, path, displayName)
          } else {
            (pathName, path, "noTag")
          }
        } else if (path.getPut != null) {
          if (path.getPut.getTags != null) {
            val displayName = openApi.getTags.asScala.filter {
              tag => tag.getName == path.getPut.getTags.asScala.head
            }.map(_.getExtensions.get("x-displayName")).head
            (pathName, path, displayName)
          } else {
            (pathName, path, "noTag")
          }
        } else if (path.getDelete != null) {
          if (path.getDelete.getTags != null) {
            val displayName = openApi.getTags.asScala.filter {
              tag => tag.getName == path.getDelete.getTags.asScala.head
            }.map(_.getExtensions.get("x-displayName")).head
            (pathName, path, displayName)
          } else {
            (pathName, path, "noTag")
          }
        } else {
          (pathName, path, "noTag")
        }
    }

    val templates = filteredPaths.toList.groupBy(_._3).flatMap {
      case (tag: String, paths: List[(String, PathItem, String)]) =>
        val baseTypeName = tag.toLowerCase.replace(" (beta)", "").stripSuffix("s")
        val endpoints: List[ScalaSwaggerEndpoint] = paths.flatMap {
          path: (String, PathItem, String) => processPath(openApi, path._2, path._1)
        }
        val baseTypes = endpoints.flatMap {
          endpoint =>
            val responseAndParamNames = (endpoint.responses.map {
              response =>
                if (response.respTypeOpt.isDefined) {
                  response.respTypeOpt.get.name
                } else {
                  null
                }
            } ++ endpoint.parameters.map {
              param => param.param.getName
            }).toSet.filter(_ != null)
            getTaggedComponents(openApi).keys.toList.intersect(responseAndParamNames.toList)
        }.toSet
        baseTypes.map {
          baseType =>
            (ScalaSwaggerTemplate(baseType, endpoints.toList), baseType)
        }
    }.toList

    templates
  }
}
