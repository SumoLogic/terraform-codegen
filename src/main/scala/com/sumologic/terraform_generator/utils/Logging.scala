package com.sumologic.terraform_generator.utils

import org.slf4j.{Logger, LoggerFactory}

trait Logging {

  @transient private var _logger = null.asInstanceOf[Logger]

  def logger: Logger = {
    if (_logger == null) {
      _logger = LoggerFactory.getLogger(getClass)
    }
    _logger
  }

  def info(msg: String, params: Any*): Unit = {
    logger.info(msg, params)
  }

  def error(msg: String, params: Any*): Unit = {
    logger.error(msg, params)
  }

  def warn(msg: String, params: Any*): Unit = {
    logger.warn(msg, params)
  }

  def debug(msg: String, params: Any*): Unit = {
    logger.debug(msg, params)
  }
}
