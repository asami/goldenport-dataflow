package org.goldenport.dataflow

import scalaz._, Scalaz._
import sql.Select

/**
 * @since   Jul. 28, 2012
 * @version Aug. 15, 2012
 * @author  ASAMI, Tomoharu
 */
sealed trait SqlBuilder {
  def toSql(): Select = {
    toSelect(Select(sql.NullRecord, sql.NullFrom))
  }
  def toSelect(select: Select): Select
}

sealed trait DataSet extends SqlBuilder {
  val tail: DataSet
  def map(f: Data => Data): DataSet = MapDataSet(this)(f)
  def flatMap(f: Data => DataSet): DataSet = FlatMapDataSet(this)(f)
  def withFilter(f: Data => Boolean): DataSet = FilterDataSet(this)(f)
  def filter(f: Data => Boolean): DataSet = FilterDataSet(this)(f)

  def toSelect(select: Select): Select = {
    to_Select(tail.toSelect(select))
  }

  protected def to_Select(select: Select): Select = {
    select
  }
}

case object NilDataSet extends DataSet {
  val tail = NilDataSet

  override def toSelect(select: Select) = {
    select
  }
}

case class TableDataSet(name: Symbol) extends DataSet {
  val tail = NilDataSet

  override def map(f: Data => Data): DataSet = {
    println("tm = " + f(TableData(this, name)))
    MappedDataSet(this, f(TableData(this, name)))
  }

  override protected def to_Select(select: Select): Select = {
    select.copy(from = sql.From(sql.QName(List(name.name))))
  }
}

case class MappedDataSet(tail: DataSet, data: Data) extends DataSet {
  override protected def to_Select(select: Select): Select = {
    data.toSelect(select)
  }
}

// XXX
case class MapDataSet(tail: DataSet)(f: Data => Data) extends DataSet {
}

// XXX
case class FlatMapDataSet(tail: DataSet)(f: Data => DataSet) extends DataSet {
}

// XXX
case class FilterDataSet(tail: DataSet)(f: Data => Boolean) extends DataSet {
}

/**
 * Data
 */
sealed trait Data extends SqlBuilder {
  val dataset: DataSet
  val tail: Data

  def update(key: Symbol, expr: Expr) = UpdateData(dataset, this, key, expr)
  def masterJoin(table: Symbol)(on: (DataExpr, DataExpr) => Predicate)(implicit policy: MasterJoinPolicy): Data = {
    MasterJoinData(dataset, this, table, on(toDataExpr, TableDataExpr(table)))
  }
  def record(fields: FieldDef*): RecordData = {
    RecordData(dataset, this, fields.toSeq)
  }
  def where(on: DataExpr => Predicate): Data = {
    WhereData(dataset, this, on(toDataExpr))
  }

  //
  def sum(key: Symbol): SumOp = SumOp(key)
  def count(key: Symbol): CountOp = CountOp(key)

  override def toSelect(select: Select) = {
    to_Select(tail.toSelect(select))
  }

  protected def to_Select(select: Select) = {
    select
  }

  def toDataExpr: DataExpr = new DataExpr() {}
}

sealed trait FieldDef
case class SymbolFieldDef(key: Symbol) extends FieldDef

case object NilData extends Data {
  val dataset = NilDataSet
  val tail = NilData

  override def toSelect(select: Select) = select
}

case class TableData(dataset: DataSet, name: Symbol) extends Data {
  val tail = NilData
}

case class SqlData(dataset: DataSet)(sql: String) extends Data {
  val tail = NilData
}

case class MasterJoinData(
  dataset: DataSet, tail: Data,
  table: Symbol,
  on: Predicate
) extends Data {
  override protected def to_Select(select: Select) = {
    select.copy(joins = select.joins :+
                sql.LeftOuterJoin(sql.QName(List(table.name)),
                                  none, on.toSql))
  }
}

case class UpdateData(dataset: DataSet, tail: Data, key: Symbol, expr: Expr) extends Data {
  override protected def to_Select(select: Select) = {
    select.copy(record = select.record.update(key.name, expr.toSql))
  }
}

case class RecordData(dataset: DataSet, tail: Data, fields: Seq[FieldDef]) extends Data {
  override protected def to_Select(select: Select) = {
    val a = fields.map(x => {
      x match {
        case f: SymbolFieldDef => sql.Column(sql.QName(List(f.key.name)))
      }
    })
    select.copy(record = select.record.update(a))
  }
}

case class WhereData(
  dataset: DataSet, tail: Data,
  on: Expr
) extends Data {
  override protected def to_Select(select: Select) = {
    select.copy(where = sql.Where(on.toSql).some)
  }
}

// XXX
case class FilterData(dataset: DataSet, tail: Data,
                      on: DataExpr => Boolean) extends Data {
}

//
sealed trait MasterJoinPolicy
case object UniqMasterJoinPolicy extends MasterJoinPolicy

/*
 * Record
 */
case class Record(fields: Seq[Record]) {
}

/*
 * Field
 */
case class Field(values: Seq[Value]) {
}

/*
 * Expr
 */
sealed trait Expr {
  def ==(rhs: Expr): EqualPredicate = EqualPredicate(this, rhs)
  def +(v: Int): PlusOp = PlusOp(this, IntValue(v))

  def toSql: sql.Expr
}

trait DataExpr extends Expr {
  def apply(key: Symbol): FieldExpr = {
    FieldExpr(key)
  }

  def toSql = sql.QName(List("Unkonwn"))
}

case class TableDataExpr(table: Symbol) extends DataExpr{
}

case class FieldExpr(key: Symbol) extends Expr {
  def toSql = sql.QName(List(key.name))
}

case class SumOp(key: Symbol) extends Expr {
  def toSql = sql.SumExpr(sql.QName(List(key.name)))
}

case class CountOp(key: Symbol) extends Expr {
  def toSql = sql.CountExpr(sql.QName(List(key.name)))
}

case class PlusOp(lhs: Expr, rhs: Expr) extends Expr {
  def toSql = sql.PlusExpr(lhs.toSql, rhs.toSql)
}

trait Value extends Expr

case class IntValue(v: Int) extends Value {
  def toSql = sql.IntExpr(v)
}

sealed trait Predicate extends Expr {
  def and(rhs: Predicate) = AndPredicate(this, rhs)
}

case class EqualPredicate(lhs: Expr, rhs: Expr) extends Predicate {
  def toSql = sql.EqualExpr(lhs.toSql, rhs.toSql)
}

case class AndPredicate(lhs: Predicate, rhs: Predicate) extends Predicate {
  def toSql = sql.AndExpr(lhs.toSql, rhs.toSql)
  
}

case class OrPredicate(lhs: Predicate, rhs: Predicate) extends Predicate {
  def toSql = sql.OrExpr(lhs.toSql, rhs.toSql)
  
}
/*
case class IntPredicate(value: Int) extends Predicate {
  def toSql = sql.IntExpr(value)
}
*/
