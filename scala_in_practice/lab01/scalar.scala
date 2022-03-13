def scalarUgly(xs: List[Int], ys: List[Int]) = {
    val length = xs.length
    var scalar_product = 0
    var i = 0
    while (i < length) {
        scalar_product += (xs(i) * ys(i))
        i += 1
    }
    scalar_product
}

def scalar(xs: List[Int], ys: List[Int]) = {
    val new_list = for ((x,y) <- (xs zip ys)) yield x * y
    new_list.reduce((a, b) => a + b)
}


//println(scalar(List(1,2,3), List(1,5,6)))
//println(scalarUgly(List(1,2,3), List(1,5,6)))
