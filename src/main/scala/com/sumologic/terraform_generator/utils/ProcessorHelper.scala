package com.sumologic.terraform_generator.utils

import java.util
import java.util.Collections

import com.sumologic.terraform_generator.StringHelper
import com.sumologic.terraform_generator.objects.TerraformModelExtensions
import io.swagger.v3.oas.models.{OpenAPI, Operation}
import io.swagger.v3.oas.models.media.{ComposedSchema, Schema}

import scala.collection.JavaConverters._

trait ProcessorHelper
  extends StringHelper
    with Logging {
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
    val propertyName = if (property.contains("(") && property.contains(")")) {
      property.split("""\(""").head
    } else {
      property
    }
    if (modelName.toLowerCase.contains("create")) {
      openAPI.getComponents.getSchemas.asScala.toList.exists {
        case (name, schema) =>
          name.toLowerCase.endsWith(modelName.toLowerCase.stripPrefix("create")) &&
            !schema.getProperties.asScala.contains(propertyName)
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

          modelsWithTag(models.filter(_.toLowerCase.contains("create")).head)._2.getProperties.containsKey(propertyName) &&
            !modelsWithTag(models.filter(_.toLowerCase.contains("update")).head)._2.getProperties.containsKey(propertyName)
        } else {
          false
        }
      } else {
        false
      }
    }
  }

  def getNameAndAttribute(property: String): (String, String) = {
    if (property.contains("(") && property.contains(")")) {
      val nameAndAttribute = property.split("""\(""")
      (nameAndAttribute.head, nameAndAttribute.last.dropRight(1))
    } else {
      (property, "")
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
    // We only care about model that have extensions (i.e. they are part of Terraform).
    // This check should be updated to look for 'x-tf-generated-properties' instead of just checking
    // for presence of extensions.
    if (model.getExtensions == null) {
      return List.empty[(String, Schema[_])]
    }

    val refProps = model match {
      case schema: ComposedSchema =>
        val allOfRefs = schema.getAllOf.asScala.map {
          child =>
            val ref = Option(child.get$ref()).getOrElse("")
            ref.split('/').last
        }.filter(_.nonEmpty)

        allOfRefs.flatMap {
          refName =>
            val (_, componentSchema) = getComponent(openAPI, refName)
            Option(componentSchema.getProperties).getOrElse(Collections.emptyMap()).asScala
        }

      case _ =>
        Seq.empty[(String, Schema[_])]
    }

    // TODO This assumes composed models will have non-ref object at the last which isn't always true.
    // see https://github.com/SumoLogic/terraform-codegen/pull/33#discussion_r447792537
    val allProps = model match {
      case schema: ComposedSchema =>
        schema.getAllOf.asScala.last.getProperties.asScala.toList ++ refProps
      case _ =>
        model.getProperties.asScala.toList ++ refProps
    }

    val propsWithAttribute = model.getExtensions.asScala(TerraformModelExtensions.Properties).toString.split(",")
    val tfPropNames = propsWithAttribute.map { prop =>
      if (prop.contains("(") && prop.contains(")")) {
        prop.split("""\(""").head
      } else {
        prop
      }
    }

    val tfProperties = allProps.filter { prop =>
      tfPropNames.contains(prop._1)
    }
    tfProperties.map {
      case (name, schema) =>
        (propsWithAttribute.filter(_.contains(name)).head, schema)
    }
  }

  def getRequiredProperties(openAPI: OpenAPI, model: Schema[_]): Seq[String] = {
    // We only care about model that have extensions (i.e. they are part of Terraform).
    // This check should be updated to look for 'x-tf-generated-properties' instead of just checking
    // for presence of extensions.
    if (model.getExtensions == null) {
      return Option(model.getRequired).getOrElse(Collections.emptyList()).asScala
    }

    val refRequiredProps = model match {
      case schema: ComposedSchema =>
        val allOfRefs = schema.getAllOf.asScala.map {
          child =>
            val ref = Option(child.get$ref()).getOrElse("")
            ref.split('/').last
        }.filter(_.nonEmpty)

        allOfRefs.flatMap {
          refName =>
            val (_, componentSchema) = getComponent(openAPI, refName)
            Option(componentSchema.getRequired).getOrElse(Collections.emptyList()).asScala
        }

      case _ =>
        Seq.empty[String]
    }

    // TODO This assumes composed models will have non-ref object at the last which isn't always true.
    // see https://github.com/SumoLogic/terraform-codegen/pull/33#discussion_r447792537
    val modelRequiredProps = model match {
      case schema: ComposedSchema =>
        schema.getAllOf.asScala.last.getRequired.asScala
      case _ =>
        Option(model.getRequired).getOrElse(Collections.emptyList()).asScala
    }

    val allRequiredProps = refRequiredProps ++ modelRequiredProps
    allRequiredProps
  }
}
