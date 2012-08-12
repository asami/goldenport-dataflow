package org.goldenport.dataflow.sql

import scalaz._, Scalaz._

/**
 * @since   Aug. 11, 2012
 * @version Aug. 13, 2012
 * @author  ASAMI, Tomoharu
 */
sealed trait Sql {
  def asString: String

  protected final def as_line(x: Option[Sql]) = {
    x.map(_.asString).fold(_ + "\n", "")
  }

  protected final def as_line(x: Seq[Sql]) = {
    x.map(_.asString).mkString("", "\n", "\n")
  }
}

case class Select(record: Record, from: From,
                  joins: Seq[Join] = Nil,
                  where: Option[Where] = None,
                  groupBy: Option[GroupBy] = None,
                  orderBy: Option[OrderBy] = None
                ) extends Sql {
  def asString = {
    "select " + record.asString + "\n" +
    from.asString + "\n" +
    as_line(joins) +
    as_line(where) + as_line(groupBy) + as_line(orderBy)
  }
}

case class Record(columns: Seq[Column]) extends Sql {
  def asString = {
    columns.map(_.asString).mkString(", ")
  }
}

case class Column(expr: Expr, name: Option[String] = None) extends Sql {
  def asString = {
    expr.asString + (name.map(" as " + _) | "")
  }
}

sealed trait Expr extends Sql {
}

case class QName(path: Seq[String]) extends Expr {
  def asString = path.mkString(".")
}

case class SubQuery(select: Select) extends Expr {
  def asString = "(" + select + ")"
}

case class EqualExpr(lhs: Expr, rhs: Expr) extends Expr {
  def asString = lhs.asString + " = " + rhs.asString
}

case class StringExpr(value: String) extends Expr {
  def asString = "'" + value + "'"
}

case class From(expr: Expr, name: Option[String] = None) {
  def asString = {
    "from " + expr.asString + (name.map(" as " + _) | "")
  }
}

trait Join extends Sql {
  def expr: Expr
  def name: Option[QName]
  def on: Expr

  protected def join_expr = {
    expr.asString + ((name.map(" as " + _.asString)) | "") + " on " + on.asString
  }
}

case class LeftOuterJoin(expr: Expr, name: Option[QName], on: Expr) extends Join {
  def asString = {
    "left outer join " + join_expr
  }
}

case class FullOuterJoin(expr: Expr, name: Option[QName], on: Expr) extends Join {
  def asString = {
    "full outer join " + join_expr
  }
}

case class Where(expr: Expr) extends Sql {
  def asString = {
    "where " + expr.asString
  }
}

case class GroupBy(names: Seq[QName]) extends Sql {
  def asString = {
    "group by " + names.map(_.asString).mkString(".")
  }
}

case class OrderBy(names: Seq[(QName, ByOrder)]) extends Sql {
  def asString = {
    "order by " + names.map(x => x._1.asString + " " + x._2).mkString(", ")
  }
}

sealed trait ByOrder
case object ASC extends ByOrder
case object DESC extends ByOrder
