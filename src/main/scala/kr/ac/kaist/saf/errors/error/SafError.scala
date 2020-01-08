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

import kr.ac.kaist.saf.errors.SafException

abstract class SafError(msg: String) extends SafException(msg)
