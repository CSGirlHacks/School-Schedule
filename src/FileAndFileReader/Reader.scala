package FileAndFileReader

import FileAndFileReader.Reader.splitIfNeeded
import scala.util.Random

object Reader {

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

  def splitSeventhGradeBand(filename:String): List[List[String]] = {
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
    val seventhBand = band.split(",").toList
    splitIfNeeded(18, seventhBand)
  }

  def splitSeventhGradeMusic(filename:String): List[List[String]] = {
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
    val seventhMusic = music.split(",").toList
    splitIfNeeded(18, seventhMusic)
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
          eighthGrade = eighthGrade + name + ","
        }
      }
    }
    eighthGrade.split(",").toList
  }

  def splitEighthGradeBand(filename:String): List[List[String]] = {
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
    val eighthBand = band.split(",").toList
    splitIfNeeded(18, eighthBand)
  }

  def splitEighthGradeMusic(filename:String): List[List[String]] = {
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
    val eighthMusic = music.split(",").toList
    splitIfNeeded(18, eighthMusic)
  }

  def splitIfNeeded(maxSize:Int, classList: List[String]) : List[List[String]] = {
    if (classList.length > maxSize) {
      val splitIndex = classList.length/2
      val classA = classList.take(splitIndex)
      val classB = classList.drop(splitIndex)
      List(classA, classB)
    }
    else{
      List(classList)
    }
  }

  def generateCohorts(filename: String): List[Cohort] = {
    val filename = "src/FileAndFileReader/Dummy Schedule.csv"
    val seventhGradeBand = splitSeventhGradeBand(filename)
    val seventhGradeBandA = new Cohort (seventhGradeBand(0))
    val seventhGradeBandB = new Cohort (seventhGradeBand(1))
    val seventhGradeMusic = splitSeventhGradeMusic(filename)
    val seventhGradeMusicA = new Cohort(seventhGradeMusic(0))
    val seventhGradeMusicB = new Cohort(seventhGradeMusic(1))
    val eighthGradeBand = splitEighthGradeBand(filename)
    val eighthGradeBandA = new Cohort(eighthGradeBand(0))
    val eighthGradeBandB = new Cohort(eighthGradeBand(1))
    val eighthGradeMusic = splitEighthGradeMusic(filename)
    val eighthGradeMusicA = new Cohort(eighthGradeMusic(0))
    val eighthGradeMusicB = new Cohort(eighthGradeMusic(1))
    val listOfCohorts = List(seventhGradeBandA, seventhGradeBandB, seventhGradeMusicA, seventhGradeMusicB, eighthGradeBandA, eighthGradeBandB, eighthGradeMusicA, eighthGradeMusicB)
    listOfCohorts
  }

def remixSchedule (input: Iterable[List[String]]) : Unit = {
    val indexList = Array(0,1, 2, 3, 4)
  val rand = new Random(System.currentTimeMillis())
  val random_index = rand.nextInt(indexList.length)
  val result = indexList(random_index)
  println(result)
  }

  def main(args: Array[String]): Unit = {
    val arrayOfCohorts = generateCohorts("src/FileAndFileReader/Dummy Schedule.csv")
    for (cohort <- arrayOfCohorts)
      cohort.getSchedule("src/FileAndFileReader/Dummy Schedule.csv")
  }
}
