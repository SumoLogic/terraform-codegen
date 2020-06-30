package com.sumologic.terraform_generator.utils

import org.slf4j.{Logger, LoggerFactory}

trait Logging {

  @transient private var _logger = null.asInstanceOf[Logger]

  def logger: Logger = {
    if (_logger == null) {
      _logger = LoggerFactory.getLogger("terraform-codegen")
    }
    _logger
  }
}
