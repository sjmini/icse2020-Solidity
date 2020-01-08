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

package kr.ac.kaist.saf.clone_detector

import kr.ac.kaist.saf.clone_detector.VectorGenerator.{ minT, name, relevants }
import kr.ac.kaist.saf.nodes.ast.ASTNode

import scala.collection.immutable.IntMap

case class Vector(vector: IntMap[Int], node_kind: Int) {
  def merge(v: Vector): Vector = {
    val nv = (vector /: v.vector) { case (v_i, (i, k)) => v_i + (i -> (v_i.getOrElse(i, 0) + k)) }
    copy(vector = nv)
  }

  def update(n: ASTNode): Vector = {
    val idx = relevants.indexOf(name(n))
    assert(idx >= 0)
    copy(vector = vector + (idx -> (vector.getOrElse(idx, 0) + 1)), node_kind = idx)
  }

  override def toString: String =
    (0 until VectorGenerator.size_of_vector).map(vector.getOrElse(_, 0)).mkString(" ")

  lazy val count: Int = vector.values.sum

  val containsEnoughTokens: Boolean = minT < count
}

object Vector {
  val empty: Vector = Vector(IntMap.empty[Int], 0)
}
