package observatory

import java.time.LocalDate
import scala.io.Source.fromFile
import java.nio.file.Paths

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val sFile = Paths.get(getClass.getResource(stationsFile).toURI).toString
    val tFile = Paths.get(getClass.getResource(temperaturesFile).toURI).toString
    val stations = fromFile(sFile).getLines().map(_.split(",", -1))
    val stationsMap = stations.filter(x => !(x(3).isEmpty || x(2).isEmpty)).map(x => ((x(0), x(1)), (x(2).toDouble, x(3).toDouble))).toMap
//    println(stationsMap.contains(("010050","")))
    val temperatures = fromFile(tFile).getLines().map(_.split(",", -1))
    temperatures.filter(x => stationsMap.contains((x(0), x(1)))).map(x => {
      val cor = stationsMap((x(0), x(1)))
      (LocalDate.of(year, x(2).toDouble.toInt, x(3).toDouble.toInt), Location(cor._1, cor._2), (x(4).toDouble - 32) / 1.8)}).toIterable
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.groupBy(x => x._2).mapValues(x => x.par.map(e => (e._3, 1)).par.reduceLeft((a, b) => (a._1 + b._1, a._2 + b._2)))
      .mapValues{case(sum: Double, num: Int) => sum / num}
  }

}
