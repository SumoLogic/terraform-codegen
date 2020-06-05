package com.sumologic.terraform_generator.objects

case class ScalaSwaggerEndpoint(endpointName: String,
                                path: String,
                                httpMethod: String,
                                parameters: List[ScalaSwaggerParameter],
                                responses: List[ScalaSwaggerResponse])
  extends ScalaTerraformEntity
    with SumoSwaggerEndpointHelper {

  case class ResponseProps(declReturnType: String, httpClientReturnType: String, responseVarDecl: String, unmarshal: String)
  def getReturnTypesBasedOnRespone(): ResponseProps = {
    val respBodyTypeOpt = this.responses.filter(_.respTypeName != "default").head.respTypeOpt
    respBodyTypeOpt match {
      case Some(respType) =>
        val writeOnlyProps = respType.props.filter(_.getCreateOnly())
        val returnHandlingPart =
          this.httpMethod.toLowerCase match {
            case "get" =>
              s"""
                |    err = json.Unmarshal(data, &${respType.name.substring(0,1).toLowerCase + respType.name.substring(1)})
                |
                |    if err != nil {
                |        return nil, err
                |    }
                |
                |    return &${respType.name.substring(0,1).toLowerCase + respType.name.substring(1)}, nil""".stripMargin
            case "post" =>
              s"""
                |    err = json.Unmarshal(data, &created${respType.name})
                |    if err != nil {
                |        return "", err
                |    }
                |
                |    return created${respType.name}.ID, nil""".stripMargin
            case "put" =>
              val writeOnlyPropsString = if (writeOnlyProps.size >= 1) {
                writeOnlyProps.map {
                  prop => s"""${respType.name.substring(0,1).toLowerCase + respType.name.substring(1)}.${prop.getName.capitalize} = """""
                }.mkString("\n    ")
              } else {
                ""
              }
              s"""
                |    ${respType.name.substring(0,1).toLowerCase + respType.name.substring(1)}.ID = ""
                |    ${writeOnlyPropsString}
                |
                |    _, err := s.Put(urlWithParams, ${respType.name.substring(0,1).toLowerCase + respType.name.substring(1)})
                |    return err""".stripMargin
            case "delete" =>
              """
                |return err""".stripMargin
            case _ =>
              ""
          }
        if (httpMethod.toLowerCase == "get") {
          ResponseProps(s"(*${respType.name}, error)", "responseBody, _, err", s"var ${respType.name.substring(0,1).toLowerCase + respType.name.substring(1)} ${respType.name}\n", returnHandlingPart)
        } else {
          if (httpMethod.toLowerCase == "post") {
            ResponseProps(s"(string, error)", "responseBody, err", s"var created${respType.name} ${respType.name}\n", returnHandlingPart)
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

  def getUrlCallBasedOnHttpMethod(): String = {
    val taggedResource = if (this.httpMethod.toLowerCase != "delete") {
      this.responses.filter {
        response => response.respTypeName != "default" && response.respTypeName != "204"
      }.head.respTypeOpt.get.name
    } else {
      ""
    }

    httpMethod.toLowerCase match {
      case "get" =>
        s"""
           |  data, _, err := s.Get(urlWithParams)
           |  if err != nil {
           |		return nil, err
           |	}
           |	if data == nil {
           |		return nil, nil
           |	}
           |""".stripMargin
      case "post" =>
        s"""
           |  data, err := s.Post(urlWithoutParams, ${lowerCaseFirstLetter(taggedResource)})
           |  if err != nil {
           |		return "", err
           |	}
           |""".stripMargin
      case "delete" =>
        s"""_, err := s.Delete(urlWithParams)"""
      case _ =>
        ""
    }
  }

  def getParamString: String = {
    val params = this.parameters
    val taggedResource = if (this.httpMethod.toLowerCase != "delete") {
      this.responses.filter {
        response => response.respTypeName != "default" && response.respTypeName != "204"
      }.head.respTypeOpt.get.name
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
              s"""if val, ok := paramMap["${lowerCaseFirstLetter(pathParam.param.getName())}"]; ok {
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
            s"""if val, ok := paramMap["${lowerCaseFirstLetter(queryParam.param.getName())}"]; ok {
               |queryParam := fmt.Sprintf("${lowerCaseFirstLetter(queryParam.param.getName())}=%s&", val)
               |paramString += queryParam
               |}
               |""".stripMargin
        }.mkString("\n")
        s"""paramString += "?"
           |
           |${queryString}
           |
           |""".stripMargin
      } else {
        ""
      }

      s"""paramString := ""
         |sprintfArgs := []interface{}{}
         |${pathParamString}
         |
         |${queryParamString}
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
        s"""if val, ok := paramMap["${lowerCaseFirstLetter(headerParam.param.getName())}"]; ok {
           |    reqHeaders["${lowerCaseFirstLetter(headerParam.param.getName())}"] = val
           |}""".stripMargin
    }.mkString("\n")

    val reqHeadersVarName = "reqHeaders"
    val reqHeaders = if (headers.isEmpty) {
        ""
      } else {
        s"""
          |$reqHeadersVarName := make(map[string]string)
          |${headers}
          |""".stripMargin
      }

    reqHeaders
  }

  override def terraformify(baseTemplate: ScalaSwaggerTemplate): String = {
    val urlWithoutParamsString =
      s"""urlWithoutParams := "${path.replaceFirst(s"\\{id\\}", "%s")}"""".replaceFirst("/", "")

    // path, query and header params
    val setParamString = getParamString
    val urlWithParamsString = if (this.httpMethod.toLowerCase == "post") {
      ""
    } else {
      """urlWithParams := fmt.Sprintf(urlWithoutParams + paramString, sprintfArgs...)"""
    }
    val setRequestHeaders = getHeaderString

    val urlCall = getUrlCallBasedOnHttpMethod()

    val response = getReturnTypesBasedOnRespone()
    val args = makeArgsListForDecl(this.parameters, baseTemplate.sumoSwaggerClassName)

    s"""
       |func (s *Client) ${this.endpointName.capitalize}($args) ${response.declReturnType} {
       |    $urlWithoutParamsString
       |    $setParamString
       |    $urlWithParamsString
       |    $setRequestHeaders
       |    $urlCall
       |    ${response.responseVarDecl}
       |    ${response.unmarshal}
       |}
       |""".stripMargin
  }
}
