
def primePairsUgly(n : Int): List[(Int, Int)] = {
    def isPrime(p : Int): Boolean = {
        if (p % 2 == 0) return false
        else {
            var i = 3
            while (i < p) {
                if (p % i == 0) return false
                else i += 2
            }
            return true
        }
    }
    
    var b = 1
    var pPairs = List[(Int, Int)]()
    do {
        var a=1
        do {
            if(isPrime(a+b)) {
                pPairs = pPairs :+ (b,a) 
                a += 1
            }
            else a += 1
        }while(a < n)
        b += 1
    }while(b<n)
    pPairs
}


def primePairs(n : Int): List[(Int, Int)] = { 
    def isPrime(p : Int): Boolean = {
        if (p % 2 == 0) return false
        else {
            for (i <- 3 to p-1 by 2) 
                if (p % i == 0) return false 
            return true
        } 
    }    
    
    val allPairs = (for(a <- 1 to n-1; b <- 1 to n-1) yield (a, b)).toList
    allPairs.filter(x => isPrime(x._1 + x._2))

}

//for (prime <- primePairs(10)) println(prime)
//for (prime <- primePairsUgly(10)) println(prime)
