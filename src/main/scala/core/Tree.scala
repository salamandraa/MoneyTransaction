package core

import scala.util.Try

sealed trait Vertex[T <: Operateable[T]] {

  def isLeaf: Boolean

  type CalculateFunction = (Operators.Value) => (T, T) => Try[T]

  def operatorsOpt: Option[Operators.Value] = {
    this match {
      case tree: Tree[T] => Some(tree.operator)
      case _: Leaf[T] => None
    }
  }

  def calculate(f: => CalculateFunction): Try[T]

  private def addRightBranch(that: T, operator: Operators.Value) = Tree(this, operator, Leaf(that))

  def +(that: T): Tree[T] = addRightBranch(that, Operators.sum)

  def -(that: T): Tree[T] = addRightBranch(that, Operators.subtraction)

  def *(that: T): Tree[T] = addRightBranch(that, Operators.multiplication)

  def /(that: T): Tree[T] = addRightBranch(that, Operators.division)

  private def mergeBranch(that: Tree[T], operator: Operators.Value) = Tree(this, operator, that)

  def +(that: Tree[T]): Tree[T] = mergeBranch(that, Operators.sum)

  def -(that: Tree[T]): Tree[T] = mergeBranch(that, Operators.subtraction)

  def *(that: Tree[T]): Tree[T] = mergeBranch(that, Operators.multiplication)

  def /(that: Tree[T]): Tree[T] = mergeBranch(that, Operators.division)
}

case class Tree[T <: Operateable[T]](leftChild: Vertex[T], operator: Operators.Value, rightChild: Vertex[T]) extends Vertex[T] {

  override def isLeaf: Boolean = false


  override def calculate(calcFun: => CalculateFunction): Try[T] = {
    val res = for {
      leftRes <- leftChild.calculate(calcFun)
      rightRes <- rightChild.calculate(calcFun)
    } yield calcFun(operator)(leftRes, rightRes)
    res.flatten
  }


  override def toString: String = "{" + leftChild + " " + Operators.toString(operator) + " " + rightChild + "}"
}

case class Leaf[T <: Operateable[T]](value: T) extends Vertex[T] {

  override def isLeaf: Boolean = true


  override def calculate(f: => CalculateFunction): Try[T] = Try(value)

  override def toString: String = value.toString

}

