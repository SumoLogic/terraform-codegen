package com.sumologic.terraform_generator.utils

import java.util.Collections

import com.sumologic.terraform_generator.objects._
import io.swagger.v3.oas.models.PathItem.HttpMethod
import io.swagger.v3.oas.models.media.{ArraySchema, ComposedSchema, Schema}
import io.swagger.v3.oas.models.parameters.{HeaderParameter, Parameter, PathParameter, QueryParameter}
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.{OpenAPI, Operation, PathItem}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.language.existentials


case class OpenApiPath(name: String, item: PathItem)

object OpenApiProcessor extends ProcessorHelper
  with Logging {

  def resolvePropertyType(openApi: OpenAPI, property: Schema[_]): ScalaSwaggerType = {
    if (property.get$ref() != null) {
      val model = getComponent(openApi, property.get$ref().split("/").last)._2
      processModel(openApi, property.get$ref(), model)
    } else {
      ScalaSwaggerType(TerraformSchemaTypes.swaggerTypeToGoType(property.getType))
    }
  }

  def processResponseObjects(openApi: OpenAPI, responses: Map[String, ApiResponse]): List[ScalaSwaggerResponse] = {
    val successResponse = responses.filter {
        // TODO create a constant for default value
        case (respName, _) => respName != "default"
      }
    // Our APIs only have a default error response and a success response
    assert(successResponse.size == 1, s"total success response=$successResponse.size")

    val (responseName, apiResponse) = successResponse.head
    val emptyResponseBody = ScalaSwaggerResponse(responseName, None)

    if (apiResponse.getContent == null) {
      return List(emptyResponseBody)
    }

    // TODO make JsonMediaType a constant
    val JsonMediaType = "application/json"
    val content = apiResponse.getContent.get(JsonMediaType)

    if (content != null && content.getSchema != null) {
      Option(content.getSchema.get$ref()) match {
        case Some(ref) =>
          val modelName = ref.split("/").last
          val (resourceName, _) = getComponent(openApi, modelName)

          val taggedModels = getTaggedComponents(openApi)
          if (taggedModels.contains(resourceName)) {
            val swaggerType = processModel(openApi, ref, taggedModels(resourceName))
            List(ScalaSwaggerResponse(resourceName, Some(swaggerType.copy(name=resourceName))))
          } else {
            logger.warn(s"model '$modelName' is not terraform resource")
            List(emptyResponseBody)
          }

        case _ =>
          throw new RuntimeException(s"Response '$responseName' has no model object")
      }
    } else {
      // No response body
      List(emptyResponseBody)
    }
  }

  def processPathParameter(openApi: OpenAPI, pathParam: PathParameter): ScalaSwaggerParameter = {
    ScalaSwaggerParameter(TerraformSupportedParameterTypes.PathParameter,
      ScalaSwaggerSimpleObject(pathParam.getName,
        ScalaSwaggerType(pathParam.getSchema.getType, List[ScalaSwaggerObject]()),
        pathParam.getRequired, Some(pathParam.getSchema.getDefault.asInstanceOf[AnyRef]), pathParam.getSchema.getDescription, ""))
  }

  def processQueryParameter(openApi: OpenAPI, queryParam: QueryParameter): ScalaSwaggerParameter = {
    ScalaSwaggerParameter(TerraformSupportedParameterTypes.QueryParameter,
      ScalaSwaggerSimpleObject(
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
      ScalaSwaggerSimpleObject(
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
      ScalaSwaggerSimpleObject(
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
      model: Schema[_] =>
        if (model.get$ref() != null) {
          processModel(openApi, model.get$ref(), model).props
        } else {
          if (model.getProperties != null) {
            val requiredProps = getRequiredProperties(openApi, model).toList
            model.getProperties.asScala.map {
              case (name, schema) =>
                processModelProperty(openApi, name, schema, requiredProps, modelDefName)
            }
          } else {
            List[ScalaSwaggerObject]()
          }
        }
    }.toList

    val swaggerType = if (modelDefName.contains("Model")) {
        ScalaSwaggerType(modelDefName.replace("Model", ""), parts.toSet.toList)
      } else {
        ScalaSwaggerType(modelDefName, parts.toSet.toList)
      }

    logger.debug(s"processed composed model='$modelDefName', swaggerType=$swaggerType")
    swaggerType
  }

  def processModel(openApi: OpenAPI, modelDefName: String, model: Schema[_]): ScalaSwaggerType = {
    val modelName = if (modelDefName.contains("/")) {
      modelDefName.split("/").last
    } else {
      modelDefName
    }

    val swaggerType = model match {
      case _: ArraySchema =>
        ScalaSwaggerType(modelName, List[ScalaSwaggerObject]())
      case composedModel: ComposedSchema =>
        processComposedModel(openApi, modelName, composedModel)
      case _ =>
        if (model.get$ref() != null) {
          // Some models have one level on indirection. Follow the ref to find out model object.
          val resourceName = model.get$ref().split("/").last
          if (getTaggedComponents(openApi).contains(resourceName)) {
            val resolvedRefModel = getTaggedComponents(openApi)(resourceName)
            processModel(openApi, modelName, resolvedRefModel)
          } else {
            processModel(openApi, modelName, getComponent(openApi, resourceName)._2)
          }
        } else {
          // Only consider properties mentioned in 'x-tf-generated-properties' extension if present.
          // Otherwise, look at all properties.
          val props: List[ScalaSwaggerObject] =
            if (model.getProperties != null) {
              val props = model.getProperties.asScala
              props.map {
                case (name, schema) =>
                  val requiredProps = List(name).intersect(getRequiredProperties(openApi, model))
                  processModelProperty(openApi, name, schema, requiredProps, modelName)
              }.toList
            } else {
              List[ScalaSwaggerObject]()
            }

          // literally only because of RoleModel
          // TODO Change name of RoleModel to Role and get rid of this code
          if (modelName.contains("Model")) {
            ScalaSwaggerType(modelName.replace("Model", ""), props.toSet.toList)
          } else {
            ScalaSwaggerType(modelName, props.toSet.toList)
          }
        }
    }

    // drop all properties except the ones specified in 'x-tf-properties' extension if present
    val tfProperties = getTerraformProperties(model)
    val filteredSwaggerType =
      if (tfProperties.isEmpty) {
        swaggerType
      } else {
        val filteredProps = swaggerType.props.filter { prop =>
          tfProperties.contains(prop.getName())
        }

        // Any terraform resource will have an 'id' property. This code assumes all terraform resource
        // model will name identifier field as 'id'. If there is no field named "id", it adds "id" as
        // extra field.
        // FIXME: This might break if we identifier field is called something else like "userId". In
        //  the case, "userId" as well as "id" will be part of that resource. Not sure how it impacts
        //  generated code. Will have to try it out and see.
        val hasIdProperty = filteredProps.exists(obj => obj.getName() == "id")
        val properties = if (hasIdProperty) {
            filteredProps
          } else {
            logger.warn(s"No id property in '$modelName'")
            filteredProps ++ List(ScalaSwaggerSimpleObject("id", ScalaSwaggerType("string"), false, None, ""))
          }
        assert(tfProperties.toSet.subsetOf(properties.map(_.getName()).toSet),
          s"Extraneous properties in x-tf-properties extension. model: $modelName, properties: " +
              s"${properties.map(_.getName())}")

        ScalaSwaggerType(swaggerType.name, properties)
      }


    logger.debug(s"processed model='$modelDefName', swaggerType=$swaggerType, filtered=$filteredSwaggerType")
    filteredSwaggerType
  }

  def processModelProperty(openApi: OpenAPI, propName: String, prop: Schema[_], requiredProps: List[String],
                           modelName: String): ScalaSwaggerObject = {

    val isWriteOnly = isPropertyWriteOnly(openApi, propName, modelName)

    val (name, attribute) = getNameAndAttribute(propName)
    if (attribute.nonEmpty && !TerraformPropertyAttributes.attributesList.contains(attribute)) {
      throw new RuntimeException(s"Invalid attribute for property: $name")
    }

    val format = Option(prop.getFormat).getOrElse("")
    val example = Option(prop.getExample).getOrElse("").toString
    val pattern = Option(prop.getPattern).getOrElse("")

    prop match {
      case arrayProp: ArraySchema =>
        val itemPattern = Option(arrayProp.getItems.getPattern).getOrElse("")

        ScalaSwaggerArrayObject(
          name,
          resolvePropertyType(openApi, arrayProp),
          requiredProps.contains(arrayProp.getName),
          None,
          prop.getDescription,
          example,
          itemPattern,
          format,
          attribute,
          isWriteOnly)

      case refProp if (refProp.get$ref() != null) =>
        val refModel = getComponent(openApi, refProp.get$ref().split("/").last)._2
        ScalaSwaggerSimpleObject(
          name,
          processModel(openApi, propName, refModel),
          // TODO is propName different from refProp.getName?
          requiredProps.contains(refProp.getName),
          None,
          refProp.getDescription,
          example,
          pattern,
          format,
          attribute,
          isWriteOnly)

      case _ =>
        ScalaSwaggerSimpleObject(
          name,
          resolvePropertyType(openApi, prop),
          requiredProps.map(_.toLowerCase).contains(propName.toLowerCase),
          Option(prop.getDefault.asInstanceOf[AnyRef]),
          prop.getDescription,
          example,
          pattern,
          format,
          attribute,
          isWriteOnly)
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
    logger.debug(s"Operation: ${operation.getOperationId} - params=$allParams, responses=$responses")
    ScalaSwaggerEndpoint(endpointName, pathName, method.name(), allParams, responses)
  }

  def processPath(openApi: OpenAPI, path: PathItem, pathName: String): List[ScalaSwaggerEndpoint] = {
    logger.debug(s"processing path: $pathName")
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

  /**
   * For each API tagged with Terraform extensions, create an Scala representation
   * object (ScalaSwaggerTemplate) capturing the CRUD endpoints.
   *
   * @param openApi - OpenAPI representation of the yaml spec.
   * @return A list of ScalaSwaggerTemplate objects.
   */
  def process(openApi: OpenAPI): List[ScalaSwaggerTemplate] = {

    val terraformPaths = filterTerraformPaths(openApi)

    val tagToPathMap = groupPathsByTag(terraformPaths)

    tagToPathMap.flatMap {
      case (tagName: String, paths: List[OpenApiPath]) =>
        val endpoints = paths.flatMap {
          path => processPath(openApi, path.item, path.name)
        }

        // Find resource type for an API. For most of the APIs, there will be only one resource type.
        // However, when inheritance is involved there can be multiple resource types i.e. each derived
        // type is a resource type.
        val baseTypes = endpoints.foldLeft(Set[String]()) {
          (types, endpoint) =>
            val responseNames = endpoint.responses.flatMap {
              response =>
                response.respTypeOpt match {
                  case Some(respType) => Some(respType.name)
                  case _ => None
                }
            }

            val paramNames = endpoint.parameters.map {
              param => param.param.getName()
            }

            val responseAndParamNames = (responseNames ++ paramNames).toSet
            // FIXME: The paramNames we get use parameter names specified in the yaml file while
            //  getTaggedComponents return x-tf-resourceName as the name of an object. So, if a
            //  body parameter is tagged as a terraform resource we won't find any baseTypes as
            //  taggedComponents name and paramNames are two different values.
            //  Either we change getTaggedComponents to return component name instead of
            //  x-tf-resourceName value or we change paramNames to x-tf-resourceName like we do
            //  for response object. My preference is to go with first option.
            val taggedComponents = getTaggedComponents(openApi).keys.toSet
            types ++ taggedComponents.intersect(responseAndParamNames)
        }
        logger.debug(s"api: $tagName, baseTypes: $baseTypes")
        assert(baseTypes.nonEmpty, s"base type for api '$tagName' is empty")

        baseTypes.map {
          baseType => ScalaSwaggerTemplate(baseType, endpoints)
        }
    }.toList
  }


  private def filterTerraformPaths(openApi: OpenAPI): Map[String, PathItem] = {
    openApi.getPaths.asScala.filter {
      case (_: String, path: PathItem) =>
        val postExtensions = if (path.getPost != null && path.getPost.getExtensions != null) {
          path.getPost.getExtensions.asScala.filterKeys(_ == "x-tf-create")
        } else {
          Map.empty[String, PathItem]
        }

        val getExtensions = if (path.getGet != null && path.getGet.getExtensions != null) {
          path.getGet.getExtensions.asScala.filterKeys(_ == "x-tf-read")
        } else {
          Map.empty[String, AnyRef]
        }

        val putExtensions = if (path.getPut != null && path.getPut.getExtensions != null) {
          path.getPut.getExtensions.asScala.filterKeys(_ == "x-tf-update")
        } else {
          Map.empty[String, AnyRef]
        }

        val deleteExtensions = if (path.getDelete != null && path.getDelete.getExtensions != null) {
          path.getDelete.getExtensions.asScala.filterKeys(_ == "x-tf-delete")
        } else {
          Map.empty[String, AnyRef]
        }

        val vendorExtensions = postExtensions ++ getExtensions ++ putExtensions ++ deleteExtensions
        vendorExtensions.nonEmpty
    }.toMap
  }

  private def groupPathsByTag(paths: Map[String, PathItem]): Map[String, List[OpenApiPath]] = {
    val tagToPathMap = mutable.Map[String, List[OpenApiPath]]()
    paths.foreach {
      case (path, pathItem) =>
        val noTag = Collections.singletonList("noTag")
        val tag = if (pathItem.getPost != null) {
          Option(pathItem.getPost.getTags).getOrElse(noTag).asScala.head
        } else if (pathItem.getGet != null) {
          Option(pathItem.getGet.getTags).getOrElse(noTag).asScala.head
        } else if (pathItem.getPut != null) {
          Option(pathItem.getPut.getTags).getOrElse(noTag).asScala.head
        } else if (pathItem.getDelete != null) {
          Option(pathItem.getDelete.getTags).getOrElse(noTag).asScala.head
        } else {
          throw new RuntimeException("Only CRUD endpoints should have terraform extensions")
        }

        val paths = tagToPathMap.getOrElse(tag, List.empty[OpenApiPath])
        tagToPathMap.update(tag, paths :+ OpenApiPath(path, pathItem))
    }

    tagToPathMap.toMap
  }
}
