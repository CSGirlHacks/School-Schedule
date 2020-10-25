package FileAndFileReader

  class Cohort (classList: List[String]) {

    def getNames(filename: String): List[String] = {
      classList
}
    def getClasses(filename: String): List[String] = {
      var listOfClasses: List[String] = List()
      val bufferedSource = io.Source.fromFile(filename)
      for (line <- bufferedSource.getLines) {
        val cols = line.split(",").map(_.trim)
        val name = (s"${cols(0)} ${cols(1)}")
        val classes = List(cols(2), cols(3), cols(4), cols(5), cols(6))
        val matches = Map(name -> classes)
        for (name <- matches.keys) {
          for (item <- classList) {
            if (name == item) {
              listOfClasses = listOfClasses ++ classes
            }
          }
        }
      }
      listOfClasses
    }


    def getSchedule(filename: String): Map[List[String],List[String]]= {
      val r= scala.util.Random
      val classes = getClasses(filename)
      val classSet = List(classes(0), classes(1), classes(2), classes(3), classes(4))
      val names = getNames(filename)
      //shuffle in literal sense shuffles the schedule
      val classNames = Map(names -> r.shuffle(classSet))
      println(classNames)
      classNames
    }
}
