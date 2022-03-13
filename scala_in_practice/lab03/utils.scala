package lab03.utils

object Utils {
    def isSorted(as: List[Int], ordering: (Int, Int) => Boolean): Boolean = {
        if(as.isEmpty) true
        else if(as.tail.isEmpty) true
        else (ordering(as.head, as.tail.head)) && isSorted(as.tail, ordering)
    }
    def isAscSorted(as: List[Int]) = {
        isSorted(as, (x, y) => x < y)
    }

    def isDescSorted(as: List[Int]) = {
        isSorted(as, (x, y) => x > y)
    }

    def foldLeft[A,B](l: List[A], z: B)(f: (B,A) => B): B = {
        if(l.isEmpty) z
        else foldLeft(l.tail, f(z, l.head))(f)
    }

    def sum(l: List[Int]): Int = {
        foldLeft(l, 0)((a, b) => a + b) 
    }

    def length(l: List[Int]): Int = {
        foldLeft(l, 0)((a, b) => a + 1) 
    }
    
    def compose[A, B, C](f: B => C, g: A => B) = {
        x: A => f(g(x))
    }

    def repeated[A](f: A => A, n: Int) = {
        def iter(f: A => A, acc: A => A, n: Int): A => A = {
            if(n == 0) acc
            else if(n % 2 == 1) iter(compose(f, f), compose(f, acc), n/2)
            else iter(compose(f,f), acc, n/2)
        }
        if(n == 1) f
        else iter(f, f, n-1)
    }

    def repeated_fold[A](f: A => A, n: Int) = {     //repeated using foldLeft
        foldLeft((2 to n).toList, f)((a, b) => compose(f,a))
    }

    def curry[A,B,C](f: (A,B) => C) = {
        x: A => y: B => f(x,y)
    }
    def uncurry[A,B,C](f: A => B => C) = {
        (x: A, y: B) => f(x)(y)
    }
    
    def unSafe[T](ex: Exception)(block: => T) = {
        try {
            block
        } catch {
            case e: Exception => {
                println("Exception caught: " + e)
                throw ex
            }
        }
    }

}


