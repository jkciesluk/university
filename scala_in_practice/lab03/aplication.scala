import lab03.utils._
import lab03.unSafe._

final case class CustomException(val message: String = "", val cause: Throwable = None.orNull) extends Exception(message, cause) 


object MyAplication extends App {
    def f(a: Int) = a * 2
    def g(a: Int) = a + 1

    def sub(a: Int, b: Int) = a - b

    val l = List(1,2,3,4)
    val l2 = List(2,5,1,3)
    val l3 = List(5,4,3,2,1)

    println(Utils.repeated(f, 4)(3))
    println(Utils.isSorted(l2, (a, b) => a > b))
    println(Utils.isAscSorted(l))
    println(Utils.compose(f,g)(4))
    println(Utils.sum(l3))
    println(Utils.curry(sub)(2)(5))

    def block(): Int = 5 / 0
    Utils.unSafe(CustomException("You can't divide by zero"))(block)
    unSafe(CustomException("Index out of bound"))({l(2)})


}

