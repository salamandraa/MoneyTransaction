package core


import scala.util.Try

trait Operateable[T] {

  def add(isRounding: Boolean)(that: T): Try[T]

  def subtraction(isRounding: Boolean)(that: T): Try[T]

  def multiplication(isRounding: Boolean)(that: T): Try[T]

  def division(isRounding: Boolean)(that: T): Try[T]

}

object Operateable {

  def calculating[T <: Operateable[T]](isRounding: Boolean)(operators: Operators.Value)(left: T, right: T): Try[T] = operators match {
    case Operators.sum => left.add(isRounding)(right)
    case Operators.subtraction => left.subtraction(isRounding)(right)
    case Operators.multiplication => left.multiplication(isRounding)(right)
    case Operators.division => left.division(isRounding)(right)
  }

}