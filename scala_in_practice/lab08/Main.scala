import  facebookAdapter._
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.Await
import java.util.concurrent.Future
import com.restfb.types.User
object Main extends App {
  FacebookAdapter.compareLikes("logs/testLogs.txt", "me", "me")
  //FacebookAdapter.compareLikes("logs/testLogs.txt", "me", "100076975779200")
}