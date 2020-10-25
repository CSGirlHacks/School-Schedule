package FileAndFileReader

object Reader {

  def convertFile(filename: String): Unit = {
    val bufferedSource = io.Source.fromFile(filename)
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      val name = (s"${cols(0)}, ${cols(1)}")
      val classes = List(cols(2), cols(3), cols(4), cols(5), cols(6))
      val matches = Map(name -> classes)
    }
  }

  def findSeventhGrade(filename:String): List[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    var seventhGrade: String = ""
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      val name = (s"${cols(0)} ${cols(1)}")
      val classes = List(cols(2), cols(3), cols(4), cols(5), cols(6))
      val matches = Map(name -> classes)
      for (entry <- matches.values){
        if (entry(1) == "MTH7") {
          seventhGrade = seventhGrade + name + ","
          }
        }
      }
    seventhGrade.split(",").toList
  }

  def splitSeventhGradeBand(filename:String): List[String] = {
    val seventhGrade = findSeventhGrade(filename)
    val bufferedSource = io.Source.fromFile(filename)
    var band = ""
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      val namesMasterList = List(cols(0) + " "  + cols(1))
      val classes = List(cols(2), cols(3), cols(4), cols(5), cols(6))
      val matches = Map(namesMasterList -> classes)
      for (allNames <- namesMasterList){
        for (name <- seventhGrade) {
          if (name == allNames) {
            if (classes(3) == "BND") {
              band = band + name + ","
            }
          }
        }
      }
    }
    band.split(",").toList
  }

  def splitSeventhGradeMusic(filename:String): List[String] = {
    val seventhGrade = findSeventhGrade(filename)
    val bufferedSource = io.Source.fromFile(filename)
    var music = ""
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      val namesMasterList = List(cols(0) + " "  + cols(1))
      val classes = List(cols(2), cols(3), cols(4), cols(5), cols(6))
      val matches = Map(namesMasterList -> classes)
      for (allNames <- namesMasterList){
        for (name <- seventhGrade) {
          if (name == allNames) {
            if (classes(3) == "MUS") {
              music = music + name + ","
            }
          }
        }
      }
    }
    music.split(",").toList
  }

  def findEighthGrade(filename:String): List[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    var eighthGrade: String = ""
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      val name = (s"${cols(0)} ${cols(1)}")
      val classes = List(cols(2), cols(3), cols(4), cols(5), cols(6))
      val matches = Map(name -> classes)
      for (entry <- matches.values){
        if (entry(1) == "MTH8") {
          eighthGrade = eighthGrade + name + ", "
        }
      }
    }
    eighthGrade.split(",").toList
  }

  def splitEighthGradeBand(filename:String): List[String] = {
    val eighthGrade = findEighthGrade(filename)
    val bufferedSource = io.Source.fromFile(filename)
    var band = ""
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      val namesMasterList = List(cols(0) + " "  + cols(1))
      val classes = List(cols(2), cols(3), cols(4), cols(5), cols(6))
      val matches = Map(namesMasterList -> classes)
      for (allNames <- namesMasterList){
        for (name <- eighthGrade) {
          if (name == allNames) {
            if (classes(3) == "BND") {
              band = band + name + ","
            }
          }
        }
      }
    }
    band.split(",").toList
  }

  def splitEighthGradeMusic(filename:String): List[String] = {
    val eighthGrade = findEighthGrade(filename)
    val bufferedSource = io.Source.fromFile(filename)
    var music = ""
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      val namesMasterList = List(cols(0) + " "  + cols(1))
      val classes = List(cols(2), cols(3), cols(4), cols(5), cols(6))
      val matches = Map(namesMasterList -> classes)
      for (allNames <- namesMasterList){
        for (name <- eighthGrade) {
          if (name == allNames) {
            if (classes(3) == "MUS") {
              music = music + name + ","
            }
          }
        }
      }
    }
    music.split(",").toList
  }

  def main(args: Array[String]): Unit = {
    val filename = "src/FileAndFileReader/Dummy Schedule.csv"
    val contents = convertFile(filename)
    val seventhGrade = findSeventhGrade(filename)
    val seventhGradeBand = splitSeventhGradeBand(filename)
    val seventhGradeMusic = splitSeventhGradeMusic(filename)
    println(seventhGradeMusic)
  }
}
