package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

trait ExtractionTest extends FunSuite {

  test("test read file") {
    val temp = Extraction.locateTemperatures(1997, "/stations.csv", "/1973.csv")
    println(temp.size)

    val average = Extraction.locationYearlyAverageRecords(temp)
  }
  
}