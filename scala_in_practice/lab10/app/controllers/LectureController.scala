package controllers
import models._
import javax.inject._
import play.api._
import play.api.mvc._


@Singleton
class LectureController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def displayLectures() = Action { implicit request: Request[AnyContent] =>
    val lectures: List[Lecture] = DB.lectures
    Ok(views.html.lectures.render(lectures))
  }
  def displayLectureById(id: Int) = Action { implicit request: Request[AnyContent] =>
    DB.lectures.find(_.id == id) match {
      case Some(lecture) => 
        val enrolled = DB.enrollments.find(_.lectureId == lecture.id)
        enrolled match {
          case Some(students) => Ok(views.html.enrolled.render(lecture, students.students))
          case None => Ok(views.html.enrolled.render(lecture, List()))
        }
      case _ => NotFound
    }
  }
  
}
