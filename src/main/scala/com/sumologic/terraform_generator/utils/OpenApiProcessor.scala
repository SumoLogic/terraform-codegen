package com.sumologic.terraform_generator.utils

import com.sumologic.terraform_generator.objects.{ScalaSwaggerEndpoint, ScalaSwaggerObject, ScalaSwaggerObjectArray, ScalaSwaggerObjectSingle, ScalaSwaggerParameter, ScalaSwaggerResponse, ScalaSwaggerTemplate, ScalaSwaggerType, TerraformSchemaTypes, TerraformSupportedParameterTypes}
import io.swagger.v3.oas.models.PathItem.HttpMethod
import io.swagger.v3.oas.models.media.{ArraySchema, ComposedSchema, Schema}
import io.swagger.v3.oas.models.parameters.{Parameter, PathParameter, QueryParameter}
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
              val resourceName = ref.split("/").toList.last
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
      ScalaSwaggerObjectSingle(queryParam.getName,
        ScalaSwaggerType(queryParam.getSchema.getType, List[ScalaSwaggerObject]()),
        queryParam.getRequired, Some(queryParam.getSchema.getDefault.asInstanceOf[AnyRef]), queryParam.getSchema.getDescription, ""))
  }

  def processBodyParameter(openApi: OpenAPI, bodyParam: Schema[_], baseType: String): List[ScalaSwaggerParameter] = {
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
            val swaggerType = processModel(openApi, modelName, resource._2)
            ScalaSwaggerParameter(TerraformSupportedParameterTypes.BodyParameter,
              ScalaSwaggerObjectSingle(modelName, swaggerType, true, None, resource._2.getDescription, ""))
          case None =>
            throw new RuntimeException("This should not happen in processBodyParameter ")
        }
    }.toList
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
      parts ++ List(ScalaSwaggerObjectSingle("id", ScalaSwaggerType("string"), false, None, "", ""))
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
            props ++ List(ScalaSwaggerObjectSingle("id", ScalaSwaggerType("string"), false, None, "", ""))
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
    val example = if (prop.getExample == null) {""} else {prop.getExample.toString}
    if (prop.isInstanceOf[ArraySchema]) {
      val arrayProp = prop.asInstanceOf[ArraySchema]
      ScalaSwaggerObjectArray(propName, resolvePropertyType(openApi, arrayProp.getItems), requiredProps.contains(arrayProp.getName), None, prop.getDescription, example, isPropertyWriteOnly(openApi, propName, modelName))
    } else {
      if (prop.get$ref() != null) {
        val refModel = getComponent(openApi, prop.get$ref().split("/").last)._2
        ScalaSwaggerObjectSingle(propName, processModel(openApi, propName, refModel), requiredProps.contains(prop.getName), None, prop.getDescription, example, isPropertyWriteOnly(openApi, propName, modelName))
      } else {
        if (propName.toLowerCase != "id") {
          ScalaSwaggerObjectSingle(propName, resolvePropertyType(openApi, prop),requiredProps.map(_.toLowerCase).contains(propName.toLowerCase), None, prop.getDescription, example, isPropertyWriteOnly(openApi, propName, modelName))
        } else {
          ScalaSwaggerObjectSingle(propName, resolvePropertyType(openApi, prop),requiredProps.map(_.toLowerCase).contains(propName.toLowerCase), None, prop.getDescription, "", isPropertyWriteOnly(openApi, propName, modelName))
        }
      }
    }
  }

  def processOperation(openApi: OpenAPI, operation: Operation, pathName: String, method: HttpMethod, baseType: String): ScalaSwaggerEndpoint = {
    val responses = processResponseObjects(openApi, operation.getResponses.asScala.toMap)

    val params: List[ScalaSwaggerParameter] = operation.getParameters.asScala.flatMap { param: Parameter =>
      param match {
        case pathParam: PathParameter =>
          List(processPathParameter(openApi, pathParam))
        case queryParam: QueryParameter =>
          List(processQueryParameter(openApi, queryParam))
        case _ =>
          if (param.getIn == null && param.getContent != null) {
            processBodyParameter(openApi, param.getSchema, baseType)
          }
          throw new RuntimeException("This should not happen in processOperation ")
      }
    }.toList

    val requestBody: List[ScalaSwaggerParameter] = if (operation.getRequestBody != null) {
      processBodyParameter(openApi, operation.getRequestBody.getContent.get("application/json").getSchema, baseType)
    } else {
      List.empty[ScalaSwaggerParameter]
    }

    val allParams = params ++ requestBody
    ScalaSwaggerEndpoint(operation.getOperationId, pathName, method.name(), allParams, responses)
  }

  def processPath(openApi: OpenAPI, path: PathItem, pathName: String, baseType: String):
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
        processOperation(openApi, operation, pathName, method, baseType)
    }.toList
  }

  def processAllClasses(openApi: OpenAPI,
                        types: List[String] = List.empty[String]): List[(ScalaSwaggerTemplate, String)] = {
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
            (ScalaSwaggerTemplate(baseType, endpoints.toList), baseType)
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
}