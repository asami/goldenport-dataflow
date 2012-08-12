package org.goldenport.dataflow.sql

import scalaz._, Scalaz._
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers

/**
 * @since   Jul. 28, 2012
 * @version Aug. 13, 2012
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SqlSpec extends WordSpec with ShouldMatchers with ScalazMatchers {
  "Sql" should {
    "asString" that {
      "simple" in {
        val s = Select(Record(List(Column(QName(List("a")), none))),
                       From(QName(List("ma")), none))
        println("SqlSpec = " + s.asString)
      }
      "full" in {
        val s = Select(Record(List(Column(QName(List("a"))),
                                   Column(QName(List("b"))))),
                       From(QName(List("ta")), Some("t")),
                       List(LeftOuterJoin(QName(List("ma")),
                                     Some(QName(List("m"))),
                                     EqualExpr(QName(List("m", "id")),
                                               QName(List("t", "id"))))),
                       Where(EqualExpr(QName(List("m", "x")), StringExpr("01"))).some,
                       GroupBy(List(QName(List("id")))).some,
                       OrderBy(List((QName(List("id")), ASC))).some)
        println("SqlSpec = " + s.asString)
      }
    }
  }
}
