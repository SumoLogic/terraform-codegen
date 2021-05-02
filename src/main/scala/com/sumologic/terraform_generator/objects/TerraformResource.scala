package com.sumologic.terraform_generator.objects

import com.sumologic.terraform_generator.objects.TerraformSupportedOperations.crud

case class TerraformResource(resourceName: String,
                             resource: OpenApiResponse,
                             endpoints: List[OpenApiEndpoint]) extends TerraformEntity {

  assert(resource.respTypeOpt.isDefined)

  def getAllTypesUsed: Set[OpenApiType] = {
    val mainClassType = resource.respTypeOpt.get

    val otherTypes = mainClassType.props.filter {
      prop => prop.getType.props.nonEmpty
    }

    val allTypes = Set(mainClassType) ++ otherTypes.map(_.getType).toSet
    allTypes
  }

  def getDataSourceFuncMappings: String = {
    val funcMappings: String = getFunctionMappings(
      List[String](TerraformSupportedOperations.READ)).mkString(",\n").concat(",")

    val resourceProps = resource.respTypeOpt.get.props.map {
      obj: OpenApiObject =>
        obj.getAsTerraformSchemaType(true)
    }.mkString(",\n").concat(",")

    s"""func dataSourceSumologic$resourceName() *schema.Resource {
       |    return &schema.Resource{
       |        $funcMappings
       |        Schema: map[string]*schema.Schema{
       |            $resourceProps
       |        },
       |    }
       |}""".stripMargin
  }

  def getFunctionName(opName: String, prefix: String): String = {
    s"$prefix${resourceName.capitalize}${opName.capitalize}"
  }

  def getFunctionMappings(interestedInOps: List[String]): List[String] = {
    interestedInOps.map {
      op: String =>
        s"${op.capitalize}: ${getFunctionName(op, "resourceSumologic")}"
    }
  }

  def getResourceType: OpenApiType = {
    resource.respTypeOpt.get
  }

  def getResourceFuncMappings: String = {
    val funcMappings: String = getFunctionMappings(
      crud.filter(_.toLowerCase != "exists")
    ).mkString(",\n").concat(",")

    val props = resource.respTypeOpt.get.props.filterNot {
      _.getName.toLowerCase.equals("id")
    }

    val propsObjects = props.map { openApiObject =>
      openApiObject.getAsTerraformSchemaType(false)
    }.toSet.mkString(",\n").concat(",")

    // Only supporting query params for now. Assuming path parameters in CRUD endpoints will only be id.
    // Not supporting header parameters yet.
    val requestMaps = endpoints.filter { endpoint =>
      val paramTypes = endpoint.parameters.map(_.paramType)
      paramTypes.exists { x =>
        x == TerraformSupportedParameterTypes.QueryParameter || x == TerraformSupportedParameterTypes.HeaderParameter
      }
    }.map {
      endpoint =>
        s"""
         |"${endpoint.httpMethod.toLowerCase}_request_map": {
         |   Type: schema.TypeMap,
         |   Optional: true,
         |   Elem: &schema.Schema{
         |       Type: schema.TypeString,
         |   },
         |}
         |""".stripMargin
    }

    val requestMapsString = if (requestMaps.nonEmpty) {
      requestMaps.mkString(",\n").concat(",")
    } else {
      ""
    }

    s"""func resourceSumologic${resourceName.capitalize}() *schema.Resource {
       |    return &schema.Resource{
       |        $funcMappings
       |        Importer: &schema.ResourceImporter{
       |            State: schema.ImportStatePassthrough,
       |        },
       |
       |        Schema: map[string]*schema.Schema{
       |            $propsObjects
       |            $requestMapsString
       |        },
       |    }
       |}""".stripMargin
  }
}
