package lab02.numbers

class Rational(numenator: Int, denominator: Int) {
    require(denominator != 0)
    def this(numenator: Int) = this(numenator, 1)
    private def gcd(a: Int, b: Int): Int = if(b ==0) a else gcd(b, a%b)
    private def lcm(a: Int, b: Int): Int = a * b / gcd(a,b)
    private def sign(a: Int): Int = if (a < 0) -1 else 1

    private val g: Int = gcd(numenator.abs, denominator.abs)
    
    val num: Int = numenator/g*sign(denominator)
    val denom: Int = denominator/g*sign(denominator)

    
    override def toString(): String = {
        val integer: Int = num / denom
        val rest: Int = (num - integer*denom).abs
        if (integer != 0 && denom != 1) s"$integer $rest/$denom"
        else if(denom == 1) num.toString
        else s"$num/$denom"
    }
    def toDouble(): Double = {
        num.toDouble / denom.toDouble
    }

    def +(other: Rational): Rational = {
        val newNum: Int = num * other.denom + other.num * denom
        val newDen: Int = denom * other.denom
        new Rational(newNum, newDen)
    }
    def +(number: Int): Rational = {
        this + Rational(number)
    }

    def -(other: Rational): Rational = {
        val newNum: Int = num * other.denom - other.num * denom
        val newDen: Int = denom * other.denom
        new Rational(newNum, newDen)
    }
    def -(number: Int): Rational = {
        this - Rational(number)
    }

    def *(other: Rational): Rational = {
        val newNum: Int = num * other.num
        val newDen: Int = denom * other.denom
        new Rational(newNum, newDen)
    }
    def *(number: Int): Rational = {
        this * Rational(number)
    }

    def /(other: Rational): Rational = {
        val newNum: Int = num * other.denom
        val newDen: Int = denom * other.num
        new Rational(newNum, newDen)
    }
    def /(number: Int): Rational = {
        this / Rational(number)
    }

    
}

object Rational {
    def apply(numenator: Int, denominator: Int = 1) = new Rational(numenator, denominator)
    val zero = new Rational(0,1)
    val one = new Rational(1,1)
}
