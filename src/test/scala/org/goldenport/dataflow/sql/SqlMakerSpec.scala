package org.goldenport.dataflow.sql

import scalaz._, Scalaz._
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.goldenport.dataflow._

/**
 * @since   Aug. 13, 2012
 * @version Aug. 15, 2012
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SqlMakerSpec extends WordSpec with ShouldMatchers with ScalazMatchers with DataFlows {
  implicit val masterjoinpolicy = UniqMasterJoinPolicy
  implicit def Int2IntOnData(v: Int) = IntValue(v)
  "SqlMaker" should {
    "1" that {
      "simple" in {
        val z = for (i <- table('T生産実績)) yield {
          val x = i.sum('原価) + 10
          val y = i.masterJoin('M部門)((x, y) => x('部門Id) == y('部門Id) and x('製品Id) == y('製品Id))
          val z = y.update('NEW_GENKA, x)
          val zz = z.where(x => x('部門Id) == 1)
          val zzz = zz.record(SymbolFieldDef('A), SymbolFieldDef('B))
          zzz
        }
        println("SqlmakerSpec = " + z.toSql.toText)
      }
    }
  }
}
