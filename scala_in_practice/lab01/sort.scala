def sortUgly(xs: List[Int]): List[Int] = {
    def partition(ys: List[Int], pivot: Int): (List[Int], List[Int]) = {
        val length = ys.length
        var i = 1
        var lower = List[Int]()
        var higher = List[Int]()
        while (i < length) {
            if(ys(i) > pivot) higher = higher :+ ys(i)
            else lower = lower :+ ys(i)
            i += 1
        }
        (lower, higher)
    }
    def quickSort(xs: List[Int]): List[Int] = {
        if (xs.length < 2) xs
        else {
            val lowerHigher = partition(xs, xs(0))
            val lower = lowerHigher._1
            val higher = lowerHigher._2
            quickSort(lower) ::: (xs(0) :: quickSort(higher))
        }   
    }

    quickSort(xs)   
}

def sort(xs: List[Int]): List[Int] = {
    val length = xs.length
    if(length < 2) xs
    else {
       val pivot = xs(0)
        
       //val lower = sort(xs filter (pivot > _))
       val lower = sort (for {x <- xs ; if (x < pivot)} yield x)
       
       //val higher = sort(xs filter (pivot < _))
       val higher = sort (for {x <- xs ; if (x > pivot)} yield x)
       
       lower ::: (pivot :: higher)
    }
}


//println(sort(List(10, 5, 7, 3, 12, 6)))
//println(sortUgly(List(10, 5, 7, 3, 12, 6)))
