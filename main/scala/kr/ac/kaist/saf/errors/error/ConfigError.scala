/**
 * *****************************************************************************
 * Copyright (c) 2018, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.saf.errors.error

sealed abstract class ConfigError(msg: String) extends SafError(msg)

case class OptAlreadyExistError(name: String) extends ConfigError({
  s"The option '$name' already exists in the option list."
})

case object OptConflictError extends ConfigError({
  s"The option list have same options."
})

case class NoChoiceError(msg: String) extends ConfigError(
  s"[NoChoiceError]: $msg"
)
