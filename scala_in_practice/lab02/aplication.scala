import lab02.numbers._
import lab02.figures._

object MyApplication extends App {
    println(Rational (1, 2) + 3)
    println(Rational(4, -5) * Rational(-4, 17))
    println(Rational(4, -5) / (Rational(3, 2) * 3))
    println(Square(Point(2,0), Point(2,2), Point(4,2), Point(4,0)))
    val pr1 = new Rectangle(new Point(Rational(1), Rational(0)),new Point(Rational(2), Rational(0)),new Point(Rational(2), Rational(4)),new Point(Rational(1), Rational(4)))
    val pr2 = new Triangle(new Point(Rational(1), Rational(0)), new Point(Rational(2), Rational(4)),new Point(Rational(1), Rational(4)))
    val pr3 = new Triangle(new Point(Rational(2, 5), Rational(0)),new Point(Rational(7, 4), Rational(4, 3)),new Point(Rational(1), Rational(12, 8)))
    val pr4 = new Rectangle(new Point(Rational(1), Rational(0)),new Point(Rational(2), Rational(0)),new Point(Rational(2), Rational(4)),new Point(Rational(1), Rational(4)))
    
    val prList = List(pr1, pr2, pr3, pr4, Square(Point(2,0), Point(2,2), Point(4,2), Point(4,0)))

    Shape.printAll(prList)
    println(Shape.areaSum(prList))

}