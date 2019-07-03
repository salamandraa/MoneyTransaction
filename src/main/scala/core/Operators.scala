package core

object Operators extends Enumeration {

  type Operators = Value
  val sum, subtraction, multiplication, division = Value

  def toString(operators: Operators.Value): String = operators match {
    case Operators.sum => "+"
    case Operators.subtraction => "-"
    case Operators.multiplication => "*"
    case Operators.division => "/"
  }

}
