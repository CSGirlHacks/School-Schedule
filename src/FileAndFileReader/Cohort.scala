package FileAndFileReader

class Cohort (classList: List[String]) {

  def getClasses(filename: String): String = {
    var listOfClasses: String = ""
    val bufferedSource = io.Source.fromFile(filename)
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      val name = (s"${cols(0)} ${cols(1)}")
      val classes = List(cols(2), cols(3), cols(4), cols(5), cols(6))
      val matches = Map(name -> classes)
      for (name <- matches.keys) {
        for (item <- classList) {
          if (name == item) {
            listOfClasses = listOfClasses + classes
          }
        }
      }
    }
   listOfClasses.split("List").toList(1)
  }

  def getSchedule(filename: String) : List[String] = {
    getClasses(filename)

  }
}





