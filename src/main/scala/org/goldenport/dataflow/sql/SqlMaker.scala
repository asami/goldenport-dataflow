package org.goldenport.dataflow.sql

import scalaz._, Scalaz._
import org.goldenport.dataflow._

/**
 * @since   Aug. 13, 2012
 * @version Aug. 14, 2012
 * @author  ASAMI, Tomoharu
 */
object SqlMaker {
  def make(ds: DataSet): Sql = {
    ds.toSql
  }
}
