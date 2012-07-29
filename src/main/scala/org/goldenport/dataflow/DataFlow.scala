package org.goldenport.dataflow

import scalaz._, Scalaz._

/**
 * @since   Jul. 28, 2012
 * @version Jul. 29, 2012
 * @author  ASAMI, Tomoharu
 */
case class DataFlow(flows: Seq[Flow]) {
  def map(x: Data => Data): DataFlow = {
    sys.error("?")
  }

  def flatMap(x: Data => DataFlow): DataFlow = {
    sys.error("?")
  }

  def withFilter(x: Data => Boolean): DataFlow = {
    sys.error("?")
  }

  def filter(x: Data => Boolean): DataFlow = {
    withFilter(x)
  }
}

object DataFlow extends DataFlows

trait DataFlows {
  def table(name: Symbol): DataSet = {
    new TableDataSet(name)
  }
}
