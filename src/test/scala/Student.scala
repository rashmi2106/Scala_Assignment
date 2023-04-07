
case class Student(id: Long, name: String)

case class Marks(subjectId: Int, studentId: Long, marksObtained: Float)

class MarksManipulation() {

  def pass_fail_count(sub_id: Long, percent: Float, pass_fail: String, source_marks: List[List[List[Marks]]]): String = {

    val fetch_list = source_marks flatMap (_ flatMap (_ map (x => check_count(x, sub_id, percent))))

    val pass_list = fetch_list.filter {
      _ == 1
    }
    val fail_list = fetch_list.filter {
      _ == 0
    }
    val passcount = pass_list.size
    val failcount = fail_list.size
    if (pass_fail.toLowerCase == "pass") s"Passcount = $passcount" else s"Failcount= $failcount"
  }

  def check_count(element: Marks, id: Long, percent: Float): Int = {

    if (id == element.subjectId && element.marksObtained > percent) 1
    else if (id == element.subjectId && element.marksObtained < percent) 0 else -1
  }

  def top_bottom(sub_id: Long, count: Int, topBottom: String, source_marks: List[List[List[Marks]]], source_student: List[Student]): String = {
    val fetch_list = source_marks flatMap (_ flatMap (_ map (x => support_top_bottom(x, sub_id))))
    val intended_list = fetch_list.filter {
      _.marksObtained != 0
    }
    val sortedList = intended_list.sortWith(_.marksObtained > _.marksObtained)
    if (topBottom.toLowerCase == "top")
      get_names(count, sortedList, source_student)
    else {
      val revSortedList = sortedList.reverse
      get_names(count, revSortedList, source_student)
    }
  }

  def get_names(count: Int, sortedList: List[Marks], source_student: List[Student]): String = {
    for (check1 <- 0 to count) {
      for (check2 <- 0 until source_student.size) {
        if (sortedList(check1).studentId == source_student(check2).id) {
          println(source_student(check2).name + " " + sortedList(check1).marksObtained + "%")
        }
      }
    }
    "These are the names!!"
  }

  def support_top_bottom(element: Marks, id: Long): Marks = {
    if (element.subjectId == id) element else Marks(0, 0, 0)
  }

  def overallTopBottom(count: Int, topBottom: String, source_marks: List[List[List[Marks]]], source_student: List[Student]): String = {

    val namesWithMarks = namesWithPercentage(source_marks, source_student)
    val sort = namesWithMarks sortBy (_._1)
    if (topBottom.toLowerCase == "bottom")
      printNames(count, sort)
    else {
      val revList = sort.reverse
      val result = printNames(count, revList)
      result
    }
  }

  def sortAccordingToStudentId(source: List[Marks]): List[Marks] = {
    source.sortWith(_.studentId < _.studentId)
  }

  def folding_function(source: List[Marks]): Double = {
    val extract = for {
      check <- 0 until source.size
    } yield source(check).marksObtained
    val listExtract = extract.toList
    val result = listExtract.foldLeft(0.0) { (first, second) => first + second }
    result
  }

  def only_names(students: List[Student]): List[String] = {
    val result = for {check <- 0 until students.size} yield students(check).name
    result.toList
  }

  def printNames(count: Int, marksNames: List[(Double, String)]): String = {
    val drop_list = marksNames.slice(0, count)
    val (marks, names) = drop_list.unzip
    for (check <- 0 until names.size)
      println(names(check) + " " + marks(check) + "%")
    "These are the names!"

  }

  def scholarship(percent: Float, amount: Long, source_marks: List[List[List[Marks]]], source_student: List[Student]): String = {

    val namesWithPercent = namesWithPercentage(source_marks, source_student)
    val sort = namesWithPercent sortBy (_._1)
    val finalNames = IsScholarship(percent, sort)
    finalNames.map(x => println(x + " " + amount))
    "Well done guyzz!!"

  }

  def IsScholarship(percent: Float, PercenageNames: List[(Double, String)]): List[String] = {
    val (percentage, names) = PercenageNames.unzip
    val isAllowed = for {
      check <- 0 until percentage.size
    } yield if (percentage(check) >= percent) check else 0
    val isAllowed_List = isAllowed.toList
    val filteredList = isAllowed_List.filter {
      _ > 0
    }
    val finalNames = for {
      check <- 0 until filteredList.size
    } yield names(filteredList(check))
    finalNames.toList
  }

  def passFailNames(passFail: String, percent: Float, source_marks: List[List[List[Marks]]], source_student: List[Student]): String = {

    val namesWithPercent = namesWithPercentage(source_marks, source_student)
    val (percentage, name) = namesWithPercent.unzip
    val indexOfNames = for {
      check <- 0 until percentage.size
    } yield if (percentage(check) >= percent) check else -1
    val indexOfNamesList = indexOfNames.toList
    if (passFail.toLowerCase == "pass") {
      val ispass = indexOfNamesList.filter(_ >= 0)
      val passNames = for {
        check <- 0 until ispass.size
      } yield name(ispass(check))
      for (check <- 0 until passNames.size) {
        println(passNames(check) + " " + percentage(ispass(check)) + "%")
      }
      "PASS STUDENT NAMES!!"
    }
    else {
      val indexOfFail = for {
        check <- 0 until percentage.size
      } yield if (percentage(check) < percent) check else -1
      val indexOfFailList = indexOfFail.toList
      val isFail = indexOfFailList.filter(_ >= 0)
      val failName = for {
        check <- 0 until isFail.size
      } yield name(isFail(check))
      for (check <- 0 until failName.size) {
        println(failName(check) + " " + percentage(isFail(check)) + "%")
      }
      "FAIL STUDENT NAMES!!"
    }
  }

  def namesWithPercentage(source_marks: List[List[List[Marks]]], source_student: List[Student]): List[(Double, String)] = {
    val fetch_list = source_marks flatMap (_ flatMap (_ map (x => x)))
    val sort_list = sortAccordingToStudentId(fetch_list)
    val aggregate = for {
      check <- 0 to fetch_list.size / 5 - 1
    } yield if (check == 0) {
      folding_function(sort_list.slice(0, 5))
    } else {
      folding_function(sort_list.slice((check * 5), (check + 1) * 5))
    }
    val aggregate_list = aggregate.toList
    val percentList = aggregate_list.map(x => x / 5)
    val names = only_names(source_student)
    percentList zip (names)
  }

  def topStudents(percent: Float, source_marks: List[List[List[Marks]]], source_student: List[Student]): String = {
    val namesWithPercent = namesWithPercentage(source_marks, source_student)
    val (percentage, name) = namesWithPercent.unzip
    val indexOfBrilliance = for {
      check <- 0 until percentage.size
    } yield if (percentage(check) >= percent) check else -1
    val indexOfBrillianceList = indexOfBrilliance.toList.filter {
      _ > 0
    }
    val rareName = for {
      check <- 0 until indexOfBrillianceList.size
    } yield name(indexOfBrillianceList(check))
    for (check <- 0 until indexOfBrillianceList.size) {
      println(rareName(check) + " " + percentage(indexOfBrillianceList(check)) + "%")
    }
    "The Gems!!"
  }
  /*def report(reportcard:String,source_marks:List[List[List[Marks]]],source_student:List[Student])=
  {
    val fetch_list=source_marks flatMap(_ flatMap(_ map(x=>x)))
    val stud_1_Name=

  }*/
}

object collection {

  def main(args: Array[String]) {

    val student_1 = Student(1, "Aman")

    val student_2 = Student(2, "Bittu")

    val student_3 = Student(3, "Ishika")

    val student_4 = Student(4, "Kanu")

    val student_5 = Student(5, "lucky")

    val student_6 = Student(6, "Mita")

    val student_7 = Student(7, "nitu")

    val student_8 = Student(8, "Om")

    val student_9 = Student(9, "Payal")

    val student_10 = Student(10, "sameer")


    val maths_1 = Marks(1, 1, 86)

    val maths_2 = Marks(1, 2, 85)

    val maths_3 = Marks(1, 3, 96)

    val maths_4 = Marks(1, 4, 76)

    val maths_5 = Marks(1, 5, 70)

    val maths_6 = Marks(1, 6, 69)

    val maths_7 = Marks(1, 7, 30)

    val maths_8 = Marks(1, 8, 38)

    val maths_9 = Marks(1, 9, 28)

    val maths_10 = Marks(1, 10, 39)


    val science_1 = Marks(2, 1, 88)

    val science_2 = Marks(2, 2, 83)

    val science_3 = Marks(2, 3, 95)

    val science_4 = Marks(2, 4, 70)

    val science_5 = Marks(2, 5, 65)

    val science_6 = Marks(2, 6, 67)

    val science_7 = Marks(2, 7, 37)

    val science_8 = Marks(2, 8, 55)

    val science_9 = Marks(2, 9, 34)

    val science_10 = Marks(2, 10, 25)


    val hindi_1 = Marks(3, 1, 85)

    val hindi_2 = Marks(3, 2, 86)

    val hindi_3 = Marks(3, 3, 94)

    val hindi_4 = Marks(3, 4, 73)

    val hindi_5 = Marks(3, 5, 68)

    val hindi_6 = Marks(3, 6, 62)

    val hindi_7 = Marks(3, 7, 40)

    val hindi_8 = Marks(3, 8, 35)

    val hindi_9 = Marks(3, 9, 44)

    val hindi_10 = Marks(3, 10, 30)


    val Eng_1 = Marks(4, 1, 86)

    val Eng_2 = Marks(4, 2, 85)

    val Eng_3 = Marks(4, 3, 97)

    val Eng_4 = Marks(4, 4, 76)

    val Eng_5 = Marks(4, 5, 70)

    val Eng_6 = Marks(4, 6, 69)

    val Eng_7 = Marks(4, 7, 30)

    val Eng_8 = Marks(4, 8, 38)

    val Eng_9 = Marks(4, 9, 28)

    val Eng_10 = Marks(4, 10, 39)


    val comp_1 = Marks(5, 1, 88)

    val comp_2 = Marks(5, 2, 83)

    val comp_3 = Marks(5, 3, 99)

    val comp_4 = Marks(5, 4, 70)

    val comp_5 = Marks(5, 5, 65)

    val comp_6 = Marks(5, 6, 67)

    val comp_7 = Marks(5, 7, 37)

    val comp_8 = Marks(5, 8, 55)

    val comp_9 = Marks(5, 9, 34)

    val comp_10 = Marks(5, 10, 25)


    val student_list = List(student_1, student_2, student_3, student_4, student_5, student_6, student_7, student_8, student_9, student_10)

    val Marks_stud_1 = List(maths_1, science_1, hindi_1, Eng_1, comp_1)

    val Marks_stud_2 = List(maths_2, science_2, hindi_2, Eng_2, comp_2)

    val Marks_stud_3 = List(maths_3, science_3, hindi_3, Eng_3, comp_3)

    val Marks_stud_4 = List(maths_4, science_4, hindi_4, Eng_4, comp_4)

    val Marks_stud_5 = List(maths_5, science_5, hindi_5, Eng_5, comp_5)

    val Marks_stud_6 = List(maths_6, science_6, hindi_6, Eng_6, comp_6)

    val Marks_stud_7 = List(maths_7, science_7, hindi_7, Eng_7, comp_7)

    val Marks_stud_8 = List(maths_8, science_8, hindi_8, Eng_8, comp_8)

    val Marks_stud_9 = List(maths_9, science_9, hindi_9, Eng_9, comp_9)

    val Marks_stud_10 = List(maths_10, science_10, hindi_10, Eng_10, comp_10)


    val marks_list = List(List(Marks_stud_1), List(Marks_stud_2), List(Marks_stud_3), List(Marks_stud_4), List(Marks_stud_5), List(Marks_stud_6), List(Marks_stud_7), List(Marks_stud_8), List(Marks_stud_9), List(Marks_stud_10))


    val obj = new MarksManipulation
    ()
    val passFail = obj.pass_fail_count(1, 40, "pass", marks_list)


    println(passFail)
    val topBottomSub = obj.top_bottom(5, 4, "bottom", marks_list, student_list)
    println(topBottomSub)
    val topBottomOverall = obj.overallTopBottom(3, "top", marks_list, student_list)
    println(topBottomOverall)
    val names = obj.scholarship(85, 2000, marks_list, student_list)
    println(names)
    val pass_Fail = obj.passFailNames("fail", 40, marks_list, student_list)
    println(pass_Fail)
    val top = obj.topStudents(95, marks_list, student_list)
    println(top)
    //obj.report("reportcard",marks_list,student_list)
  }

}
