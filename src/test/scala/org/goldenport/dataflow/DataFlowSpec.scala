package org.goldenport.dataflow

import scalaz._, Scalaz._
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers

/**
 * @since   Jul. 28, 2012
 * @version Aug. 15, 2012
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DataFlowSpec extends WordSpec with ShouldMatchers with ScalazMatchers with DataFlows {
  implicit val masterjoinpolicy = UniqMasterJoinPolicy
  "DataFlow" should {
    "1" that {
      "1-1" in {
        val z = for (i <- table('T生産実績)) yield {
          println("i = " + i)
          val x = i.sum('原価) + 10
          val y = i.masterJoin('M部門)((x, y) => x('部門Id) == y('部門Id) and x('製品Id) == y('製品Id))
          val z = y.update('NEW_GENKA, x)
          val zz = z.where(x => x('部門Id) == IntValue(1))
          val zzz = zz.record(SymbolFieldDef('A), SymbolFieldDef('B))
          println("zzz = " + zzz)
          zzz
        }
        println("z = " + z)
      }
    }
  }
  "2" that {
    
  }
}
