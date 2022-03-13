package lab03.unSafe

object unSafe{
    def apply[T](ex: Exception)(block: => T) = {
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


