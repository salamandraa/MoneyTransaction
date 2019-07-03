package core

import java.util.{Currency, Locale}

import scala.util.Try

case class Money(amount: BigDecimal, currency: Currency) extends Operateable[Money] {

  private val isValidMoney = !(isAmountNegative || isAmountNotValidCurencyUnit)

  private def isAmountNegative = amount < 0

  private def isAmountNotValidCurencyUnit = !(Math.pow(10, defaultFractionDigits) * amount).isValidLong

  //  require(amount >= 0, "Currency must not be negative")
  //  require((Math.pow(10, defaultFractionDigits) * amount).isValidLong, "Currency must be a multiple of the minimum currency unit")

  private def defaultFractionDigits: Int = currency.getDefaultFractionDigits


  def inOtherCurrency(otherCurrency: Currency, isRounding: Boolean = false): Try[Money] = {
    if (isValidMoney) {
      ExchangerCurrency.exchanger.exchangeMoney(this, otherCurrency, isRounding)
    } else {
      Try(throw new ExceptionInInitializerError(toStringIfError))
    }
  }


  private def calculate(isRounding: Boolean)(that: Money, operators: Operators.Value): Try[Money] = {

    /**
      * Округление не происходит , если сконструируется не валидная сумма, то бросится исключение
      */
    def toCommonMoney(isRounding: Boolean)(that: Money): Try[Money] = ExchangerCurrency.exchanger.exchangeMoney(that, currency, isRounding)

    if (isValidMoney) {
      for {
        newMoneyFromThat <- toCommonMoney(isRounding)(that)
        amountAfterCalulate = operators match {
          case Operators.sum => amount + newMoneyFromThat.amount
          case Operators.subtraction => amount - newMoneyFromThat.amount
          case Operators.multiplication => amount * newMoneyFromThat.amount
          case Operators.division => amount / newMoneyFromThat.amount
        }
      } yield copy(amount = amountAfterCalulate)
    } else {
      Try(throw new RuntimeException(toStringIfError))
    }

  }


  override def add(isRounding: Boolean)(that: Money): Try[Money] = calculate(isRounding)(that, Operators.sum)

  override def subtraction(isRounding: Boolean)(that: Money): Try[Money] = calculate(isRounding)(that, Operators.subtraction)

  override def multiplication(isRounding: Boolean)(that: Money): Try[Money] = calculate(isRounding)(that, Operators.multiplication)

  override def division(isRounding: Boolean)(that: Money): Try[Money] = calculate(isRounding)(that, Operators.division)

  override def toString: String = if (isValidMoney) {
    "Money(" + amount + "," + currency + ")"
  } else {
    toStringIfError
  }

  def toStringIfError: String = {
    val res = if (isAmountNegative) {
      "Amount must not be negative : this.amount =" + amount
    }
    else if (isAmountNotValidCurencyUnit) {
      "Currency must be a multiple of the minimum currency unit, amount=" + amount + ", mde=" + Math.pow(10, -defaultFractionDigits)
    } else {
      "Эта строчка не должна была вывестись"
    }
    "Money(Error: "+res+")"
  }
}

object Money {

  def apply(amount: BigDecimal, currency: Currency): Money = {
    val newMoney = new Money(amount, currency)
    if (!newMoney.isValidMoney) {
      println(newMoney.toStringIfError)
    }
    newMoney
  }

  implicit def toLeaf(money: Money): Leaf[Money] = Leaf(money)

  val calculatingWithRounding: Operators.Value => (Money, Money) => Try[Money] = Operateable.calculating[Money](isRounding = true)

  val calculatingNoRounding: Operators.Value => (Money, Money) => Try[Money] = Operateable.calculating[Money](isRounding = false)

  val rusCurrency: Currency = Currency.getInstance("RUB")

  def ruble(amount: BigDecimal): Money = Money(amount, rusCurrency)

  def dollar(amount: BigDecimal): Money = Money(amount, Currency.getInstance(Locale.US))

  def euro(amount: BigDecimal): Money = Money(amount, Currency.getInstance(Locale.GERMANY))

}