package FileAndFileReader

object Reader {

  def convertFile(filename: String): Unit = {
    val bufferedSource = io.Source.fromFile(filename)
    for (line <- bufferedSource.getLines) {
      var cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      val name = "${cols(0)}, ${cols(1)}"
      val classes = List(cols(2), cols(3), cols(4), cols(5), cols(6))
      val matches = Map(name -> classes)
      println(matches)
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "FileAndFileReader/Dummy Schedule.csv"
    val contents = convertFile(filename)
    println(contents)
  }
}
