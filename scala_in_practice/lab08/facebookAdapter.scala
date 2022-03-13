package  facebookAdapter

import com.restfb.DefaultFacebookClient
import com.restfb.types.User
import com.restfb.Version
import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure
import java.io.FileWriter
import java.io.File
import java.util.Calendar

object FacebookAdapter {
    implicit val executor = scala.concurrent.ExecutionContext.global
    private val myAppSecret = "5f3d4cf636e12215405e60c1788cc9e0"
    private val userToken = "EAARdiSR9e3kBAKrkI6LBVDy72XT87D1PLALMLCOv35gRvJcL0pJOHpaCN0jyiqZAyjbSbqLm6vALeZBhCIyitNUAhqoXGx8dLZAScvDgthRuEw7ZAJAnNICCUZBFPr7jQk2BNdGFVYWodHbk4mSKhDcqEUv92fGZBoGwtLS1C3hhZCMGlqbHvHK97PGRcXt36jnRipH1xlCR6QhfivaxR2lTtiE3NFDKG12vZCdJndrJEoQIVEJ2XEjt"
    class MyFacebookClient(currentAccessToken: String)
        extends DefaultFacebookClient(
            currentAccessToken,
            myAppSecret,
            Version.VERSION_5_0
            )
            

    
    def getUser(accessToken: String = userToken, id: String): Future[User] = Future {
        val client = new MyFacebookClient(accessToken)
        val user = client.fetchObject(id, classOf[User])
        user
    }

    def userName(user: User): String = user.getName()

    def getLikes(user: User): Long = user.getLikes() match {
        case null => 0
        case likes => likes.getTotalCount()
    }

    def writeToFile(path: String, text: String): Unit = {
        val file = new File(path)
        file.createNewFile()
        val fw = new FileWriter(path, true)
        try {
        fw.write(text)
        fw.write(System.getProperty( "line.separator" ))
        } catch {
            case e: Throwable => println("Error while writing to file " + e)
        }
        finally fw.close() 
    }

    def compareLikes(logFile: String, user1: String, user2: String): Unit = {
        val u1Future: Future[User] = getUser(id = user1)
        val u2Future: Future[User] = getUser(id = user2)
        val allLikes = u1Future.zip(u2Future)
        allLikes onComplete{
            case Success(value) => {
                val u1: User = value._1
                val u2: User = value._2
                
                println(s"${userName(u1)}, likes: ${getLikes(u1)} vs. ${userName(u2)}, likes: ${getLikes(u2)}")
                writeToFile(logFile, s"${Calendar.getInstance().getTime()} ${userName(u1)}: ${getLikes(u1)}, ${userName(u2)}: ${getLikes(u2)}")
            }
            case Failure(exception) => println("An error has occurred: " + exception.getMessage)
        }
    }
}