package com.sumologic.terraform_generator.objects

object TerraformPathTags {
  final val Create = "x-tf-create"
  final val Read = "x-tf-read"
  final val Update = "x-tf-update"
  final val Delete = "x-tf-delete"

  val tagsList = List(Create, Read, Update, Delete)
}

object TerraformModelExtensions {
  final val Properties = "x-tf-generated-properties"
  final val ResourceName = "x-tf-resource-name"
}

object TerraformPropertyAttributes {
  final val UNIQUE = "unique"

  val attributesList = List(UNIQUE)
}

object TerraformSupportedOperations {
  final val CREATE = "create"
  final val GET = "get"
  final val UPDATE = "update"
  final val DELETE = "delete"
  final val EXISTS = "exists"

  val crud = List(CREATE, GET, UPDATE, DELETE, EXISTS)
}

object ForbiddenGoTerms {
  final val TYPE = "type"

  val forbidden = List(TYPE)
}

object TerraformSchemaTypes {
  final val TypeBool = "schema.TypeBool"
  final val TypeInt = "schema.TypeInt"
  final val TypeFloat = "schema.TypeFloat"
  final val TypeString = "schema.TypeString"
  // Date is represented using TypeString
  final val TypeMap = "schema.TypeMap"
  final val TypeList = "schema.TypeList"
  final val TypeSet = "schema.TypeSet"

  def openApiTypeToTerraformSchemaType(openApiType: String): String = {
    openApiType match {
      case "string" => TypeString
      case "int" => TypeInt
      case "bool" => TypeBool
      case "float" => TypeFloat // TODO does float exist in swagger?
      case "array" => TypeList
      case _ => TypeString // TODO this generalization is way too naive
      //TODO Date type? translates to TypeString
    }
  }

  def openApiTypeToGoType(openApiType: String): String = {
    openApiType match {
      case "boolean" => "bool"
      case "object" => "map[string]string"
      case "integer" => "int"
      // TODO eventually we want to replace this method with OpenApiObject.getGoType
      case "array" => throw new RuntimeException("Should not be called for array objects")
      case _ => openApiType
    }
  }

  def openApiTypeToPlaceholder(openApiType: String): String = {
    openApiType match {
      case "bool" => "%t"
      case "array" => "%v"
      case "int" => "%d"
      case _ => "\"%s\""
    }
  }
}

object TerraformSupportedParameterTypes {
  final val PathParameter = "PathParameter"
  final val BodyParameter = "BodyParameter"
  final val QueryParameter = "QueryParameter"
  final val HeaderParameter = "HeaderParameter"
}
