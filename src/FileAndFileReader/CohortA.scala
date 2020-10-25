package FileAndFileReader

object CohortA {
  val filename = "src/FileAndFileReader/Dummy Schedule.csv"
  val seventhMusic = Reader.splitIfNeeded(18, Reader.splitSeventhGradeMusic(filename))

  def main(args: Array[String]): Unit = {
    println(seventhMusic)
  }
}
