package observatory


import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers {

//  test("test predict temperature") {
//    val temp = Extraction.locateTemperatures(1973, "/stations.csv", "/1973.csv")
//
//    val average = Extraction.locationYearlyAverageRecords(temp)
//    val predict = Visualization.predictTemperature(average, Location(90, 180))
//    println(predict)
//  }
//
//  test("test color interpolate") {
//    val temp: Iterable[(Temperature, Color)] = Iterable((60, Color(255, 255, 255)), (32, Color(255, 0, 0)), (12, Color(255, 255, 0)),
//      (0, Color(0, 255, 255)), (-15, Color(0, 0, 255)), (-27, Color(255, 0, 255)),  (-50, Color(33, 0, 107)), (-60, Color(0, 0, 0)))
//    println(Visualization.interpolateColor(temp, 30))
//    assert(Visualization.interpolateColor(temp, 32) == Color(255, 0, 0))
//    assert(Visualization.interpolateColor(temp, 60) == Color(255, 255, 255))
//    assert(Visualization.interpolateColor(temp, 22) == Color(255, 127, 0))
//  }

  test("output image") {
    val colors: Iterable[(Temperature, Color)] = Iterable((60, Color(255, 255, 255)), (32, Color(255, 0, 0)), (12, Color(255, 255, 0)),
      (0, Color(0, 255, 255)), (-15, Color(0, 0, 255)), (-27, Color(255, 0, 255)),  (-50, Color(33, 0, 107)), (-60, Color(0, 0, 0)))
    val temp = Extraction.locateTemperatures(1973, "/stations.csv", "/1975.csv")
    println("the data have size " + temp.size)
    val average = Extraction.locationYearlyAverageRecords(temp)
    Visualization.visualize(average, colors)
  }

}
