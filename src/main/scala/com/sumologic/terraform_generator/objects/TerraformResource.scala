package com.sumologic.terraform_generator.objects

import com.sumologic.terraform_generator.objects.TerraformSupportedOperations.crud

case class TerraformResource(resourceName: String,
                             endpoints: List[OpenApiEndpoint]) extends TerraformEntity {

  def getAllTypesUsed: Set[OpenApiType] = {
    // FIXME: There is only one tf resource. We should add it as class member of TerraformResource instead of
    //  traversing all endpoints.
    val apiResponse = endpoints.flatMap { endpoint =>
      endpoint.responses.find { response =>
        response.respTypeName == resourceName
      }
    }.head
    assert(apiResponse.respTypeOpt.isDefined, s"OpenApiType missing for ${apiResponse.respTypeName}")

    val mainClassType = apiResponse.respTypeOpt.get

    val otherTypes = mainClassType.props.filter {
      prop => prop.getType.props.nonEmpty
    }

    Set(mainClassType) ++ otherTypes.map(_.getType).toSet
  }

  // FIXME: Get rid of this method. There is only one terraform resource and we already know about that.
  //  Don't look at create and update request response models, instead make resource a data member of
  //  TerraformResource class.
  def getUpdateAndCreateRequestBodyType: List[OpenApiType] = {
    val apiEndpoints = endpoints.filter { op: OpenApiEndpoint =>
      op.endpointName.equalsIgnoreCase(TerraformSupportedOperations.UPDATE + resourceName) ||
        op.endpointName.equalsIgnoreCase(TerraformSupportedOperations.CREATE + resourceName) ||
        op.responses.map(_.respTypeName).contains(resourceName)
    }

    val types = apiEndpoints.flatMap { endpoint =>
      val filteredParams = endpoint.parameters.filter(_.paramType == TerraformSupportedParameterTypes.BodyParameter)
      filteredParams.flatMap(_.param.getAllTypes) ++
          endpoint.responses.filter(_.respTypeOpt.isDefined).map(_.respTypeOpt.get)
    }

    types.filter(_.name.toUpperCase.equals(resourceName.toUpperCase())).distinct
  }

  def getDataSourceFuncMappings: String = {
    val funcMappings: String = getFunctionMappings(
      List[String](TerraformSupportedOperations.GET)).mkString(",\n").concat(",")

    val mainClass = getMainObjectClass

    val mainClassProps = mainClass.props.map {
      obj: OpenApiObject =>
        obj.getAsTerraformSchemaType(true)
    }.mkString(",\n").concat(",")

    s"""func dataSourceSumologic$resourceName() *schema.Resource {
       |    return &schema.Resource{
       |        $funcMappings
       |        Schema: map[string]*schema.Schema{
       |            $mainClassProps
       |        },
       |    }
       |}""".stripMargin
  }


  def getFunctionName(opName: String, prefix: String): String = {
    val camelOp = opName.substring(0, 1).toUpperCase() + opName.substring(1)
    if (camelOp == "Get") {
      s"$prefix${resourceName.capitalize}Read"
    } else {
      s"$prefix${resourceName.capitalize}$camelOp"
    }
  }

  def getFunctionMappings(interestedInOps: List[String]): List[String] = {
    interestedInOps.map {
      op: String =>
        val camelOp = op.substring(0, 1).toUpperCase() + op.substring(1)
        if (camelOp == "Get") {
          s"Read: ${getFunctionName(op, "resourceSumologic")}"
        } else {
          s"$camelOp: ${getFunctionName(op, "resourceSumologic")}"
        }
    }
  }

  def getMainObjectClass: OpenApiType = {
    val typesUsed: Set[OpenApiType] = getAllTypesUsed

    typesUsed.find {
      t => t.name.toUpperCase == resourceName.toUpperCase() ||
           t.name.toUpperCase.contains(resourceName.toUpperCase)
    }.getOrElse {
      throw new RuntimeException("No Main Class. This should not happen in getMainObjectClass ")
    }
  }

  def getResourceFuncMappings: String = {
    val funcMappings: String = getFunctionMappings(
      crud.filter(_.toLowerCase != "exists")
    ).mkString(",\n      ").concat(",")

    // TODO This assumption is too optimistic
    val classes = getUpdateAndCreateRequestBodyType

    val classesProps = classes.flatMap {
      _.props.filter { prop =>
        !prop.getName.toLowerCase.equals("id")
      }
    }.toSet

    val propsObjects = classesProps.map { openApiObject =>
      openApiObject.getAsTerraformSchemaType(false)
    }.toList.toSet.mkString(",\n").concat(",")

    // Only supporting query params for now. Assuming path parameters in CRUD endpoints will only be id.
    // Not supporting header parameters yet.
    val requestMaps = endpoints.filter { endpoint =>
      val paramTypes = endpoint.parameters.map(_.paramType)
      paramTypes.exists { x =>
        x == TerraformSupportedParameterTypes.QueryParameter || x == TerraformSupportedParameterTypes.HeaderParameter
      }
    }.map {
      endpoint =>
          """
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
