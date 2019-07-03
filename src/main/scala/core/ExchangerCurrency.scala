package core

import java.util.{Currency, Locale}
import scala.math.BigDecimal.RoundingMode
import scala.util.Try


case class ExchangerCurrency(currencyFrom: Currency, currencyTo: Currency) {
  def swapFromToCurrency = ExchangerCurrency(currencyTo, currencyFrom)

  def isFromEqualsTo: Boolean = currencyTo == currencyFrom
}

object ExchangerCurrency {

  val exchanger: Exchanger = Exchanger()
    .addExchangerCurrency(ExchangerCurrency(Money.rusCurrency, Currency.getInstance(Locale.US)), 0.01472)
    .addExchangerCurrency(ExchangerCurrency(Money.rusCurrency, Currency.getInstance(Locale.GERMANY)), 0.01258, Some(79.4164))
    .addExchangerCurrency(ExchangerCurrency(Currency.getInstance(Locale.US), Currency.getInstance(Locale.GERMANY)), 0.8577)


  case class Exchanger(map: Map[ExchangerCurrency, BigDecimal] = Map.empty) {
    /**
      * Обменик денег
      *
      * @param exchangerCurrency    Какую валюту хотим поменять , какую валюту хотим получить
      * @param crossCourseFromTo    кросс курс moneyAmountCurrencyFrom* crossCourseFromTo= moneyAmountCurrencyTo
      * @param crossCourseToFromOpt курс обмена из второй валюты в первую
      * @return Новый обменник с добавлеными кросс курсами валют
      */
    def addExchangerCurrency(exchangerCurrency: ExchangerCurrency, crossCourseFromTo: BigDecimal, crossCourseToFromOpt: Option[BigDecimal] = None): Exchanger = {
      val forwardExchange = exchangerCurrency -> crossCourseFromTo
      val reverseExchange = crossCourseToFromOpt match {
        case Some(crossCourseToFrom) => exchangerCurrency.swapFromToCurrency -> crossCourseToFrom
        case None => exchangerCurrency.swapFromToCurrency -> 1 / crossCourseFromTo
      }
      val newMap = map + (forwardExchange, reverseExchange)
      Exchanger(newMap)
    }

    def exchangeMoney(money: Money, currencyTo: Currency, isRounding: Boolean = false): Try[Money] = Try {
      val exchangerCurrency = ExchangerCurrency(money.currency, currencyTo)
      if (exchangerCurrency.isFromEqualsTo) {
        money //если валюты одинаковы возвращаем то же самое , что пришло
      }
      else {
        val crossCourseOpt = map.get(exchangerCurrency)
        crossCourseOpt match {
          case Some(crossCourse) =>
            val amountWithoutRounding = money.amount * crossCourse
            val actualAmount = if (isRounding) {
              amountWithoutRounding.setScale(currencyTo.getDefaultFractionDigits, RoundingMode.DOWN)
            }
            else {
              amountWithoutRounding
            }
            Money(actualAmount, currency = currencyTo)
          case None => throw new NoSuchElementException("Нет кросс курса из валюты " + exchangerCurrency.currencyFrom + " в валюту " + exchangerCurrency.currencyTo)
        }
      }
    }
  }


}