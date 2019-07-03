import java.util.{Currency, Locale}

import core.Money


import Money._

//TODO !!!!! Скопировано из TestMoney.sc !!!!! там видны результаты
// переводим доллары в рубли и обратно
val dollarMuch = Money.dollar(1000000000)
val rubleFromDollar = dollarMuch.inOtherCurrency(Money.rusCurrency, isRounding = true)
val dollarFromRuble = rubleFromDollar.flatMap(_.inOtherCurrency(Currency.getInstance(Locale.US), isRounding = true))


val dollar10 = Money.dollar(10)
val dollar20 = Money.dollar(20)
val dollar30 = Money.dollar(30)
val dollar40 = Money.dollar(40)

// проверяем цепочку операций и вычисляем результат
val result1 = dollar10 * dollar20 + dollar30 / dollar40


result1.calculate(Money.calculatingWithRounding)

// проверяем работу со скобками, проверяем цепочку операций и вычисляем результат
val result2 = dollar10 * (dollar20 + dollar30) / dollar40


val result3 = result2.calculate(Money.calculatingWithRounding)


//переводим в другую валюту
result3.flatMap(_.inOtherCurrency(Currency.getInstance(Locale.GERMANY), isRounding = true))


val rubl30_01 = Money.dollar(30.01)
val euro10 = Money.euro(10)


//работа с несколькими валютами
val expressionThree = dollar10 * rubl30_01 + rubl30_01 - euro10


val dollarError  = Money.dollar(10.00001)

val errorResult = dollarError+dollar30

errorResult.calculate(Money.calculatingWithRounding)
