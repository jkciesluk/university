package lab02.figures

import lab02.numbers.Rational

class Point(val x: Rational, val y: Rational) {
  def this(x: Int, y: Int) = this(Rational(x), Rational(y))
  def distance(other: Point): Double = {
      val xDist: Rational = x - other.x
      val yDist: Rational = y - other.y
      val d: Rational = xDist*xDist + yDist*yDist

      scala.math.sqrt(d.toDouble)
  }
}

object Point{
  def apply(x: Rational, y: Rational) = new Point(x, y)
  def apply(x: Int, y: Int) = new Point(Rational(x), Rational(y))
}

abstract class Shape {
    def area: Double
    val description: String
    override def toString = s"$description, area: $area"
}

class Rectangle (val a: Point, val b: Point, val c: Point, val d: Point) extends Shape {
  val ab = a distance b
  val ad = a distance d
  val bc = b distance c
  val cd = c distance d
  require((ad == bc) && (ab == cd))
  def area: Double = ab * bc
  val description = "Rectangle"
}

object Rectangle {
  def apply(a: Point, b: Point, c: Point, d: Point) = new Rectangle(a, b, c, d)
}


class Square (val a: Point, val b: Point, val c: Point, val d: Point) extends Shape {
  val ab = a distance b
  val ad = a distance d
  val bc = b distance c
  val cd = c distance d
  require((ad == bc) && (ab == cd) && (ad == ab))
  def area: Double = ab * ab
  override val description = "Square"
}

object Square {
  def apply(a: Point, b: Point, c: Point, d: Point) = new Square(a, b, c, d)
}


class Triangle (val a: Point, val b: Point, val c: Point) extends Shape {
  val ab = a distance b
  val bc = b distance c
  val ac = a distance c
  require((ab + bc > ac) && (ac + ab > bc) && (bc + ac > ab))
  def area: Double = {
    val p = (ab + bc + ac) / 2
    scala.math.sqrt((p * (p - ab) * (p - ac) * (p - bc)).toDouble)
  }
  val description = "Triangle"
}

object Triangle {
  def apply(a: Point, b: Point, c: Point) = new Triangle(a, b, c)
}



object Shape {
  def printAll(figures: List[Shape]): Unit = {
    for (figure <- figures) println(figure.description)
  }
  def areaSum(figures: List[Shape]): Double = {
    figures.foldLeft(0.0)((a,b) => a + b.area)
  }
}
    