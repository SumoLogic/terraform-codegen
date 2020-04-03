package com.sumologic.terraform_generator.objects

import com.sumologic.terraform_generator.StringHelper

trait SumoSwaggerEndpointHelper extends StringHelper {
  def bodyParamToArgList(bodyParamOption: Option[SumoSwaggerParameter]): List[String] = {
    bodyParamOption match {
      case Some(bodyParam) =>
        val requestBodyType = bodyParam.param.getType().name
        List[String](lowerCaseFirstLetter(requestBodyType) + " " + requestBodyType)
      case None => List[String]()
    }
  }

  def paramListToArgList(params: List[SumoSwaggerParameter]): List[String] = {
    params.map(_.toTerraformFuncArg())
  }

  def makeArgsListForDecl(params: List[SumoSwaggerParameter]): String = {
    val allParams = getArgsListForDecl(params)
    allParams.mkString(", ")
  }

  def getArgsListForDecl(params: List[SumoSwaggerParameter]): List[String] = {
    // TODO val queryParamList = params.filter(_.paramType == SumoTerraformSupportedParameterTypes.QueryParameter)
    val requestMap = if (params.map(_.paramType).contains(SumoTerraformSupportedParameterTypes.QueryParameter)) {
      List("paramMap map[string]string")
    } else {
      List.empty[String]
    }

    // TODO Need to consider required params
    val pathParamList = params.filter(
      _.paramType == SumoTerraformSupportedParameterTypes.PathParameter)

    val bodyParamOpt = params.find(_.paramType == SumoTerraformSupportedParameterTypes.BodyParameter)

    if (bodyParamOpt.isDefined) {
      bodyParamToArgList(bodyParamOpt) ++ requestMap
    } else {
      paramListToArgList(pathParamList) ++ requestMap
    }
  }
}
