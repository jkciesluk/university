import lab07._
object MyApplication extends App {
    val m1 = Money(100, EUR)
    val m2 = Money(400, PLN)
    println(m1 + m2)

println(100("$") + 25(PLN))
println(200(USD) as PLN)
println(100("€") > 100(USD))
println(100("zl") * 20.3)
println(23.50(EUR) as "zl")
//println(200("dollars"))
}