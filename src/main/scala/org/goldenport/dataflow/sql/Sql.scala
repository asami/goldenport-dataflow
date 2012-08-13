package org.goldenport.dataflow.sql

import scalaz._, Scalaz._

/**
 * @since   Aug. 11, 2012
 * @version Aug. 13, 2012
 * @author  ASAMI, Tomoharu
 */
sealed trait Sql {
  def toText: String

  protected final def as_line(x: Option[Sql]) = {
    x.map(_.toText).fold(_ + "\n", "")
  }

  protected final def as_line(x: Seq[Sql]) = {
    x.map(_.toText).mkString("", "\n", "\n")
  }
}

case class Select(record: Record, from: From,
                  joins: Seq[Join] = Nil,
                  where: Option[Where] = None,
                  groupBy: Option[GroupBy] = None,
                  orderBy: Option[OrderBy] = None
                ) extends Sql {
  def toText = {
    "select " + record.toText + "\n" +
    from.toText + "\n" +
    as_line(joins) +
    as_line(where) + as_line(groupBy) + as_line(orderBy)
  }
}

case class Record(columns: Seq[Column]) extends Sql {
  def toText = {
    columns.map(_.toText).mkString(", ")
  }
}

object NullRecord extends Record(Nil)

case class Column(expr: Expr, name: Option[String] = None) extends Sql {
  def toText = {
    expr.toText + (name.map(" as " + _) | "")
  }
}

sealed trait Expr extends Sql {
}

case class QName(path: Seq[String]) extends Expr {
  def toText = path.mkString(".")
}

case class SubQuery(select: Select) extends Expr {
  def toText = "(" + select + ")"
}

case class EqualExpr(lhs: Expr, rhs: Expr) extends Expr {
  def toText = lhs.toText + " = " + rhs.toText
}

case class StringExpr(value: String) extends Expr {
  def toText = "'" + value + "'"
}

object NullExpr extends Expr {
  def toText = sys.error("not implemented yet.")
}

case class From(expr: Expr, name: Option[String] = None) {
  def toText = {
    "from " + expr.toText + (name.map(" as " + _) | "")
  }
}

object NullFrom extends From(NullExpr)

trait Join extends Sql {
  def expr: Expr
  def name: Option[QName]
  def on: Expr

  protected def join_expr = {
    expr.toText + ((name.map(" as " + _.toText)) | "") + " on " + on.toText
  }
}

case class LeftOuterJoin(expr: Expr, name: Option[QName], on: Expr) extends Join {
  def toText = {
    "left outer join " + join_expr
  }
}

case class FullOuterJoin(expr: Expr, name: Option[QName], on: Expr) extends Join {
  def toText = {
    "full outer join " + join_expr
  }
}

case class Where(expr: Expr) extends Sql {
  def toText = {
    "where " + expr.toText
  }
}

case class GroupBy(names: Seq[QName]) extends Sql {
  def toText = {
    "group by " + names.map(_.toText).mkString(".")
  }
}

case class OrderBy(names: Seq[(QName, ByOrder)]) extends Sql {
  def toText = {
    "order by " + names.map(x => x._1.toText + " " + x._2).mkString(", ")
  }
}

sealed trait ByOrder
case object ASC extends ByOrder
case object DESC extends ByOrder
