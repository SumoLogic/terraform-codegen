package com.sumologic.terraform_generator.utils

import com.sumologic.terraform_generator.StringHelper
import io.swagger.v3.oas.models.{OpenAPI, Operation}
import io.swagger.v3.oas.models.media.{ComposedSchema, Schema}

import scala.collection.JavaConverters._

trait ProcessorHelper extends StringHelper {
  def getTaggedComponents(openAPI: OpenAPI): Map[String, Schema[_]] = {
    val componentsWithExtensions = openAPI.getComponents.getSchemas.asScala.toList.filter {
      schema =>
        schema._2.getExtensions != null
    }.toMap

    componentsWithExtensions.map {
      case (name, schema) =>
        if (schema.getExtensions.asScala.contains("x-tf-resource-name")) {
          (schema.getExtensions.asScala("x-tf-resource-name").toString, schema)
        } else {
          (name, schema)
        }
    }.toMap

  }

  // Checks if this prop can only be set in a create request and cannot be updated
  def isPropertyWriteOnly(openAPI: OpenAPI, property: String, modelName: String): Boolean = {
    if (modelName.toLowerCase.contains("create")) {
      openAPI.getComponents.getSchemas.asScala.toList.exists {
        case (name, schema) =>
          name.toLowerCase.endsWith(modelName.toLowerCase.stripPrefix("create")) &&
            !schema.getProperties.asScala.contains(property)
      }
    } else {
      val modelsWithTag: Map[String, (String, Schema[_])] = getTagForComponent(openAPI, modelName)

      if (modelsWithTag.contains(modelName)) {
        val tag = modelsWithTag(modelName)._1
        val baseType = tag.replace("Management", "")
        val groupedByTag = modelsWithTag.groupBy(_._2._1)
        if (groupedByTag(tag).keys.toList.count {
          model => model.toLowerCase.contains(baseType.toLowerCase) &&
            (model.toLowerCase.contains("create") || model.toLowerCase.contains("update"))
        } == 2) {
          val models = groupedByTag(tag).keys.toList.filter {
            model => model.toLowerCase.contains(baseType.toLowerCase) &&
              (model.toLowerCase.contains("create") || model.toLowerCase.contains("update"))
          }

          modelsWithTag(models.filter(_.toLowerCase.contains("create")).head)._2.getProperties.containsKey(property) &&
            !modelsWithTag(models.filter(_.toLowerCase.contains("update")).head)._2.getProperties.containsKey(property)
        } else {
          false
        }
      } else {
        false
      }
    }
  }

  private def getTagForComponent(openAPI: OpenAPI, modelName: String): Map[String, (String, Schema[_])] = {
    openAPI.getComponents.getSchemas.asScala.map {
      case (name, schema) =>
        // for each component, figure out the tag by the path that this component appears in
        val paths = openAPI.getPaths.asScala.toList.flatMap{
          case (pathName, path) => List(path.getGet, path.getPost, path.getPut, path.getDelete)
        }.filter {
          op => if (op != null) {
            (doesOperationRequestBodyContainModel(name, op) || doesOperationResponseContainModel(name, op))
          } else {
            false
          }
        }
        val tag = if (paths.nonEmpty) {
          paths.map(_.getTags.asScala.head).toSet.head
        } else {
          ""
        }

        (name, (tag, schema))
    }.toMap
  }

  private def doesOperationRequestBodyContainModel(modelName: String, operation: Operation): Boolean = {
    if(operation.getRequestBody != null && operation.getRequestBody.getContent != null && operation.getRequestBody.getContent.get("application/json") != null
      && operation.getRequestBody.getContent.get("application/json").getSchema != null) {
      operation.getRequestBody.getContent.get("application/json").getSchema.get$ref().contains(modelName)
    } else {
      false
    }
  }

  private def doesOperationResponseContainModel(modelName: String, operation: Operation): Boolean = {
    if(operation.getResponses != null) {
      operation.getResponses.asScala.toList.exists {
        response => if (response._2.getContent != null && response._2.getContent.get("application/json") != null && response._2.getContent.get("application/json").getSchema != null
          && response._2.getContent.get("application/json").getSchema.get$ref() != null) {
          response._2.getContent.get("application/json").getSchema.get$ref().contains(modelName)
        } else {
          false
        }
      }
    } else {
      false
    }
  }

  def getComponent(openAPI: OpenAPI, name: String): (String, Schema[_]) = {
    val component = openAPI.getComponents.getSchemas.asScala.getOrElse(name,
      throw new Exception(s"Unexpected error. Missing schema for $name") // throw a better exception
    )

    if (component.getExtensions != null && component.getExtensions.asScala.contains("x-tf-resource-name")) {
      (component.getExtensions.asScala("x-tf-resource-name").toString, component)
    } else {
      (name, component)
    }
  }

  def getTaggedProperties(openAPI: OpenAPI, model: Schema[_]): List[(String, Schema[_])] = {
    if (model.getExtensions != null) {
      val propNames = model.getExtensions.asScala("x-tf-generated-properties").toString.split(",").toList
      val refProps = if (model.isInstanceOf[ComposedSchema]) {
        val allOfRefs = model.asInstanceOf[ComposedSchema].getAllOf.asScala.map{
          child =>
            if (child.get$ref() != null) {
              child.get$ref().split("/").last
            } else {
              null
            }
        }.filter(_ != null)
        allOfRefs.flatMap(getComponent(openAPI, _)._2.getProperties.asScala.toList).toList
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
        allOfRefs.flatMap(getComponent(openAPI, _)._2.getRequired.asScala).toList
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
}
