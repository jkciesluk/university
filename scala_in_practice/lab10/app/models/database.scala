package models


object DB {
  val students = List(
    Student(0, "Aleks", 3),
    Student(1, "Anna", 2),
    Student(2, "Kamil", 1),
    Student(3, "Przemek", 2),
    Student(4, "Denis", 3),
    Student(5, "Mateusz", 2),
    Student(6, "Zuzanna", 3),
    Student(7, "Patryk", 1),
    Student(8, "Jakub", 2),
    Student(9, "Kacper", 1)
  )
  val lectures = List(
    Lecture(0, "Scala in Practice"),
    Lecture(1, "Metody programowania"),
    Lecture(2, "Algebra liniowa"),
    Lecture(3, "Analiza numeryczna")
  )
  val enrollments = List(
    Enrollment(0, students.filter(_.year > 1)),
    Enrollment(1, students.filter(_.year == 2)),
    Enrollment(2, students.filter(_.name.startsWith("K"))),
    Enrollment(0, students.filter(_.year == 3)) 
  )
}