package com.sumologic.terraform_generator.objects

object TerraformPathTags {
  final val CREATE = "x-tf-create"
  final val READ = "x-tf-read"
  final val UPDATE = "x-tf-update"
  final val DELETE = "x-tf-delete"

  val tagsList = List(CREATE, READ, UPDATE, DELETE)
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

  def swaggerTypeToTerraformSchemaType(swaggerType: String): String = {
    swaggerType match {
      case "string" => TypeString
      case "int" => TypeInt
      case "bool" => TypeBool
      case "float" => TypeFloat // TODO does float exist in swagger?
      case "array" => TypeList
      case _ => TypeString // TODO this generalization is way too naiive
      //TODO Date type? translates to TypeString
    }
  }

  def swaggerTypeToGoType(swaggerType: String, format: String = null): String = {
    swaggerType match {
      case "boolean" => "bool"
      case "object" => "map[string]string"
      case "array" => "[]string"
      case "integer" => "int"
      case _ => swaggerType
    }
  }

  def swaggerTypeToPlaceholder(swaggerType: String): String = {
    swaggerType match {
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