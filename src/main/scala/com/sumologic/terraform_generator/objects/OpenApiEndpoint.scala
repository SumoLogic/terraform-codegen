package com.sumologic.terraform_generator.objects

case class OpenApiEndpoint(endpointName: String,
                           endpointType: String,
                           path: String,
                           httpMethod: String,
                           parameters: List[OpenApiParameter],
                           responses: List[OpenApiResponse])
  extends TerraformEntity
    with OpenApiEndpointUtil {

  case class ResponseProps(declReturnType: String,
                           httpClientReturnType: String,
                           responseVarDecl: String,
                           unmarshal: String)

  def getReturnTypesBasedOnResponse: ResponseProps = {
    val respBodyTypeOpt = this.responses.filter(_.respTypeName != "default").head.respTypeOpt
    respBodyTypeOpt match {
      case Some(respType) =>
        val returnHandlingPart = this.httpMethod.toLowerCase match {
          case "get" =>
            // TODO: Make this a util method like 'toCamelCase'
            val resourceVar = respType.name.head.toLower + respType.name.substring(1)
            s"""
              |    err = json.Unmarshal(data, &$resourceVar)
              |    if err != nil {
              |        return nil, err
              |    }
              |
              |    return &$resourceVar, nil
              |""".stripMargin

          case "post" =>
            s"""
              |    err = json.Unmarshal(data, &created${respType.name})
              |    if err != nil {
              |        return "", err
              |    }
              |
              |    return created${respType.name}.ID, nil
              |""".stripMargin

          case "put" =>
            """
              |    return err
              |""".stripMargin

          case "delete" =>
            """
             |    return err
             |""".stripMargin

          case _ =>
            throw new Exception(s"HTTP method can only be Post, Get, Put, or Delete")
        }

        if (httpMethod.toLowerCase == "get") {
          ResponseProps(s"(*${respType.name}, error)",
            "responseBody, _, err",
            s"var ${respType.name.head.toLower + respType.name.substring(1)} ${respType.name}\n",
            returnHandlingPart
          )
        } else {
          if (httpMethod.toLowerCase == "post") {
            ResponseProps(s"(string, error)",
              "responseBody, err",
              s"var created${respType.name} ${respType.name}\n",
              returnHandlingPart
            )
          } else {
            ResponseProps(s"error", "responseBody, err", "", returnHandlingPart)
          }
        }

      case None =>
        val returnHandlingPart =
          """return err""".stripMargin
        ResponseProps("error", "_, err", "", returnHandlingPart)
    }
  }

  def getUrlCallBasedOnHttpMethod(urlArg: String): String = {
    val taggedResource = if (this.httpMethod.toLowerCase != "delete") {
      this.responses.filter {
        response => response.respTypeName != "default" && response.respTypeName != "204"
      }.head.respTypeOpt.map(_.name).getOrElse("")
    } else {
      ""
    }

    val varName = if (taggedResource.isEmpty) {
      "nil"
    } else {
      lowerCaseFirstLetter(taggedResource)
    }

    httpMethod.toLowerCase match {
      case "get" =>
        s"""
           |    data, _, err := s.Get($urlArg)
           |    if err != nil {
           |	 		  return nil, err
           |	 	}
           |	 	if data == nil {
           |	 		  return nil, nil
           |	 	}
           |""".stripMargin

      case "post" =>
        s"""
           |    data, err := s.Post($urlArg, $varName)
           |    if err != nil {
           |	 		  return "", err
           |	 	}
           |""".stripMargin

      case "delete" =>
        s"""_, err := s.Delete($urlArg)"""

      case "put" =>
        // unset write only properties before invoking update on the resource.
        val responseTypeOpt = this.responses.head.respTypeOpt
        assert(responseTypeOpt.isDefined)
        val respType = responseTypeOpt.get

        val writeOnlyProps = responseTypeOpt.get.props.filter(_.getCreateOnly)
        val writeOnlyPropsString = if (writeOnlyProps.nonEmpty) {
          writeOnlyProps.map { prop =>
            s"""
               |${respType.name.head.toLower + respType.name.substring(1)}.${prop.getName.capitalize} = ""
               |""".stripMargin
          }.mkString("\n    ")
        } else {
          ""
        }

        val resourceVar = respType.name.head.toLower + respType.name.substring(1)
        s"""
           |    ${resourceVar}.ID = ""
           |    $writeOnlyPropsString
           |
           |    _, err := s.Put($urlArg, $resourceVar)
           |""".stripMargin

      case _ =>
        throw new Exception(s"HTTP method can only be Post, Get, Put, or Delete")
    }
  }

  def getParamString: String = {
    val params = this.parameters
    val taggedResource = if (this.httpMethod.toLowerCase != "delete") {
      this.responses.filter {
        response => response.respTypeName != "default" && response.respTypeName != "204"
      }.head.respTypeOpt.map(_.name).getOrElse("")
    } else {
      ""
    }

    val pathParams = params.filter {
      param => param.paramType == TerraformSupportedParameterTypes.PathParameter
    }
    val queryParams = params.filter {
      param => param.paramType == TerraformSupportedParameterTypes.QueryParameter
    }

    if(queryParams.nonEmpty || pathParams.nonEmpty) {
      val pathParamString = if (pathParams.nonEmpty) {
        pathParams.map {
          pathParam =>
            if (pathParam.param.getName.toLowerCase == "id") {
              if (this.httpMethod.toLowerCase == "put") {
                s"""sprintfArgs = append(sprintfArgs, ${lowerCaseFirstLetter(taggedResource)}.ID)
                   |""".stripMargin
              } else {
                s"""sprintfArgs = append(sprintfArgs, id)
                   |""".stripMargin
              }
            } else {
              s"""if val, ok := paramMap["${lowerCaseFirstLetter(pathParam.param.getName)}"]; ok {
                 | sprintfArgs = append(sprintfArgs, val)
                 | }
                 |""".stripMargin
            }
        }.mkString("\n")
      } else {
        ""
      }

      val queryParamString = if (queryParams.nonEmpty) {
        val queryString = queryParams.map {
          queryParam =>
            s"""if val, ok := paramMap["${lowerCaseFirstLetter(queryParam.param.getName)}"]; ok {
               |queryParam := fmt.Sprintf("${lowerCaseFirstLetter(queryParam.param.getName)}=%s&", val)
               |paramString += queryParam
               |}
               |""".stripMargin
        }.mkString("\n")
        s"""paramString += "?"
           |
           |$queryString
           |
           |""".stripMargin
      } else {
        ""
      }

      s"""paramString := ""
         |sprintfArgs := []interface{}{}
         |$pathParamString
         |
         |$queryParamString
         |
         |""".stripMargin
    } else {
      ""
    }
  }

  def getHeaderString: String = {
    val headerParams = this.parameters.filter {
      param => param.paramType == TerraformSupportedParameterTypes.HeaderParameter
    }

    val headers = headerParams.map {
      headerParam =>
        s"""if val, ok := paramMap["${lowerCaseFirstLetter(headerParam.param.getName)}"]; ok {
           |    reqHeaders["${lowerCaseFirstLetter(headerParam.param.getName)}"] = val
           |}""".stripMargin
    }.mkString("\n")

    val reqHeadersVarName = "reqHeaders"
    val reqHeaders = if (headers.isEmpty) {
        ""
      } else {
        s"""
          |$reqHeadersVarName := make(map[string]string)
          |$headers
          |""".stripMargin
      }

    reqHeaders
  }

  override def terraformify(resource: TerraformResource): String = {
    val urlWithoutParamsString =
      s"""urlWithoutParams := "${path.replaceFirst(s"\\{id\\}", "%s")}"""".replaceFirst("/", "")

    // path, query and header params
    val paramString = getParamString
    val urlWithParamsString = if (paramString.isEmpty) {
      ""
    } else {
      """urlWithParams := fmt.Sprintf(urlWithoutParams + paramString, sprintfArgs...)"""
    }
    val setRequestHeaders = getHeaderString

    val urlArg = if (paramString.isEmpty) "urlWithoutParams" else "urlWithParams"
    val urlCall = getUrlCallBasedOnHttpMethod(urlArg)

    val response = getReturnTypesBasedOnResponse
    val args = makeArgsListForDecl(this.parameters, resource.resourceName)

    s"""
       |func (s *Client) ${this.endpointName.capitalize}($args) ${response.declReturnType} {
       |    $urlWithoutParamsString
       |    $paramString
       |    $urlWithParamsString
       |    $setRequestHeaders
       |    $urlCall
       |    ${response.responseVarDecl}
       |    ${response.unmarshal}
       |}
       |""".stripMargin
  }
}
