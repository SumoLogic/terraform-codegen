package com.sumologic.terraform_generator.objects

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

object TerraformSupportedParameterTypes {
  final val PathParameter = "PathParameter"
  final val BodyParameter = "BodyParameter"
  final val QueryParameter = "QueryParameter"
}