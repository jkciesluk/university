import  scala.io.Source

def fileLinesUgly(file: java.io.File): List[String] = {
    var lines = Source.fromFile(file).getLines().toList
    var i = 0
    val length = lines.length
    var newLines = List[String]()
    while (i < length) {
        newLines = newLines :+ s"${lines(i)}\n"
        i+= 1
    }
    newLines
}

def fileLines(file: java.io.File): List[String] = {
    (for(line <- Source.fromFile(file).getLines()) yield line + "\n").toList
    //(Source.fromFile(file).getLines()).toList     //The same, but without adding newline sign at the end of each line 
}

//val filesHere = new java.io.File(".").listFiles
//for (line <- fileLinesUgly(filesHere(0))) print(line)
//for (line <- fileLines(filesHere(0))) print(line)