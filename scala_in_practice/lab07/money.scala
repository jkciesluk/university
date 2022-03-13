package object lab07 {
import scala.language.implicitConversions

trait Currency
case object USD extends Currency {override def toString() = "USD"}
case object PLN extends Currency {override def toString() = "PLN"}
case object EUR extends Currency {override def toString() = "EUR"}

val conversion: Map[(Currency, Currency), BigDecimal] = {
    Map((EUR, USD) -> 1.13, 
        (USD, EUR) -> 0.88,
        (PLN, USD) -> 0.24, 
        (USD, PLN) -> 4.10,
        (PLN, EUR) -> 0.22,
        (EUR, PLN) -> 4.64)
    }

case class CurrencyConverter(conversion: Map[(Currency, Currency), BigDecimal]) {
    def convert(from: Currency, to: Currency): BigDecimal = conversion.find(pair => pair._1._1 == from & pair._1._2 == to).getOrElse(((from, to), BigDecimal(1)))._2
}
implicit val currencyConverter: CurrencyConverter = CurrencyConverter(conversion)


case class Money(amount: BigDecimal, currency: Currency)(implicit currencyConverter: CurrencyConverter) {
    override def toString(): String = f"$amount($currency)"
    def +(right: Money): Money = {
        val newRight: BigDecimal = right.amount * currencyConverter.convert(right.currency, this.currency)
        Money(this.amount + newRight, this.currency)
    }
    def -(right: Money): Money = {
        val newRight: BigDecimal = right.amount * currencyConverter.convert(right.currency, this.currency)
        Money(newRight, this.currency)
    }
    def *(right: BigDecimal): Money = {
        Money(this.amount * right, this.currency)
    }
    
    def as(right: Currency): Money = {
        Money(this.amount * currencyConverter.convert(this.currency, right), right)
    }

    def >(right: Money): Boolean = {
        val newRight: BigDecimal = right.amount * currencyConverter.convert(right.currency, this.currency)
        this.amount > newRight
    }
    def <(right: Money): Boolean = {
        val newRight: BigDecimal = right.amount * currencyConverter.convert(right.currency, this.currency)
        this.amount < newRight
    }
    def ==(right: Money): Boolean = {
        val newRight: BigDecimal = right.amount * currencyConverter.convert(right.currency, this.currency)
        this.amount == newRight
    }
}


implicit class MyInt(amount: Int) {
    def apply(currency: Currency): Money =  Money(amount, currency)
}
implicit class MyDouble(amount: Double) {
    def apply(currency: Currency): Money =  Money(amount, currency)
}


implicit val string2Currency: String => Currency = {
        case "zl" => PLN
        case "$" => USD
        case "€" => EUR
        case "PLN" => PLN
        case "USD" => USD
        case "EUR" => EUR
        case _ => throw new Exception("Unknown currency")
}


}

//println(100(EUR) + 25(PLN) + 20(USD)