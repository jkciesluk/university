package controllers
import models._
import javax.inject._
import play.api._
import play.api.mvc._


@Singleton
class StudentController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def displayStudents() = Action { implicit request: Request[AnyContent] =>
    val students: List[Student] = DB.students
    Ok(views.html.students.render(students))
  }
  def displayStudentByIndex(index: Int) = Action { implicit request: Request[AnyContent] =>
    DB.students.find(_.index == index) match {
      case Some(student) => Ok(views.html.studentByIndex.render(student))
      case None => NotFound
    }
  }
  
}
