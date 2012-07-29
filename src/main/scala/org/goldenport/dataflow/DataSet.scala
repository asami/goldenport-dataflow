package org.goldenport.dataflow

import scalaz._, Scalaz._

/**
 * @since   Jul. 28, 2012
 * @version Jul. 29, 2012
 * @author  ASAMI, Tomoharu
 */
sealed trait DataSet {
  val tail: DataSet
  def map(f: Data => Data): DataSet = MapDataSet(this)(f)
  def flatMap(f: Data => DataSet): DataSet = FlatMapDataSet(this)(f)
  def withFilter(f: Data => Boolean): DataSet = FilterDataSet(this)(f)
  def filter(f: Data => Boolean): DataSet = FilterDataSet(this)(f)
}

case object NillDataSet extends DataSet {
  val tail = NillDataSet
}

case class MapDataSet(tail: DataSet)(f: Data => Data) extends DataSet {
}

case class FlatMapDataSet(tail: DataSet)(f: Data => DataSet) extends DataSet {
}

case class FilterDataSet(tail: DataSet)(f: Data => Boolean) extends DataSet {
}

/**
 * Data
 */
sealed trait Data {
  val dataset: DataSet
  val tail: Data

  def update(key: Symbol, expr: Expr) = UpdateData(dataset, this)(key, expr)
  def masterJoin(table: Symbol)(on: (DataOnData, DataOnData) => OnData)(implicit policy: MasterJoinPolicy): Data = {
    MasterJoinData(dataset, this)
  }

  //
  def sum(key: Symbol): SumOp = SumOp(key)
  def count(key: Symbol): CountOp = CountOp(key)
}

case object NilData extends Data {
  val tail = NilData
}

case class TableData(dataset: DataSet)(name: Symbol) extends Data {
  val tail = NilData
}

case class SqlData(dataset: DataSet)(sql: String) extends Data {
  val tail = NilData
}

case class MasterJoinData(dataset: DataSet, tail: Data) extends Data {
}

case class UpdateData(dataset: DataSet, tail: Data)(key: Symbol, expr: Expr) extends Data

//
sealed trait OnData {
  def and(rhs: OnData) = AndOnData(this, rhs)
}

case class DataOnData() extends OnData {
  def apply(key: Symbol): FieldOnData = {
    FieldOnData(key)
  }
}

case class FieldOnData(key: Symbol) extends OnData {
  def ==(rhs: FieldOnData): EqualOnData = EqualOnData(this, rhs)
}

case class EqualOnData(lhs: OnData, rhs: OnData) extends OnData

case class AndOnData(lhs: OnData, rhs: OnData) extends OnData

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
  def +(v: Int): PlusOp = PlusOp(this, Value(v))
}  

case class SumOp(key: Symbol) extends Expr {
}

case class CountOp(key: Symbol) extends Expr {
}

case class PlusOp(lhs: Expr, rhs: Expr) extends Expr

case class Value(v: Int) extends Expr
