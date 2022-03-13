package lab05.plugins

abstract class Pluginable {
    def apply(text: String): String = text    
}

trait Reverting extends Pluginable{ 
    abstract override def apply(text: String): String = {
        super.apply(text.reverse)
    }
}



trait LowerCasing extends Pluginable{
    abstract override def apply(text: String): String = {
        super.apply(text.toLowerCase)
    }
}

trait SingleSpacing extends Pluginable{
    abstract override def apply(text: String): String = {
        super.apply(text.replaceAll(" +", " "))
    }
} 


trait NoSpacing extends Pluginable{
    abstract override def apply(text: String): String = {
        super.apply(text.replaceAll(" +", ""))
    }
}
trait DuplicateRemoval extends Pluginable{
    abstract override def apply(text: String): String = {
        super.apply(text.flatMap(letter => {
            if(text.indexOf(letter) == text.lastIndexOf(letter)) letter.toString
            else ""
        }))
    }
}

trait Rotating extends Pluginable{
    abstract override def apply(text: String): String = {
        super.apply(f"${text.last}${text.dropRight(1)}")
    }
} 

trait Doubling extends Pluginable{
    abstract override def apply(text: String): String = {
        super.apply(text.sliding(2,2).flatMap(x => if (x.length > 1) f"${x(0)}${x(1)}${x(1)}" else x).mkString)
    }
}

trait Shortening extends Pluginable{
    abstract override def apply(text: String): String = {
        super.apply(text.sliding(2,2).flatMap(x => if (x.length > 1) f"${x(0)}" else x).mkString)
    }
}



object Actions {
    def andThen(first: Pluginable, second: Pluginable) = new Pluginable {
            override def apply(text: String): String = (first.apply  _ andThen second.apply)(text)
    }
    
    val actionA: Pluginable = new Pluginable with Shortening with Doubling with SingleSpacing

    val actionB: Pluginable = new Pluginable with Doubling with Shortening with NoSpacing

    val rotating: Pluginable = new Pluginable with Rotating 

    val actionC: Pluginable = new Pluginable with Doubling with LowerCasing

    val actionD: Pluginable = new Pluginable with Rotating with DuplicateRemoval

    val actionE: Pluginable = new Pluginable with Reverting with Doubling with Shortening with NoSpacing 

    val actionF: Pluginable = andThen(andThen(andThen(rotating, rotating),andThen(rotating, rotating)), rotating)

    val actionG: Pluginable = andThen(actionA, actionB)
}

