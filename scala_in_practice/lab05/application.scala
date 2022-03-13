import lab05.plugins._

object MyApplication extends App {
    println(Actions.actionA("a   bc     d e fgh")) // => a bc d e fgh => a  bcc dd ee ffghh => a c de fh
    println(Actions.actionB("a   bc     d e fgh")) // => abcdefgh => aceg => accegg
    println(Actions.actionC("AbbAAC"))             // => abbaac => abbbaaacc
    println(Actions.actionD("abhbccdefgf"))        // => ahdeg => gahde
    println(Actions.actionE("a  b cd  ef"))        // => abcdef => ace => acce => ecca
    println(Actions.actionF("123456789"))          // => => => => => 567891234
    println(Actions.actionG("a   bc     d e fgh")) // => a c de fh => addf
}