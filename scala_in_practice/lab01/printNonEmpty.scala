import  scala.io.Source

def printNonEmptyUgly(pattern: String): Unit = {
    val filesHere = new java.io.File(".").listFiles
    val length = filesHere.length
    var i = 0 
    while (i < length) {
        
        if ((filesHere(i).getName endsWith pattern) && (Source.fromFile(filesHere(i)).size > 0)){
            println(filesHere(i).getName)
        }
        i += 1
    }
    
}
def printNonEmpty(pattern: String): Unit = {
    val filesHere = new java.io.File(".").listFiles
    for (file <- filesHere.filter(x => (x.getName endsWith pattern) && 
                                        Source.fromFile(x).size > 0)) {
        println(file.getName)
    }
}

//printNonEmpty(".scala")
//printNonEmptyUgly(".scala")