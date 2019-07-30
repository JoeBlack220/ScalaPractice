package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math._
/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  val INIT_TEMP = -99999999
  def computeDistance(l1: Location, l2: Location) : Double = {
    if(l1.lat == l2.lat && l1.lon == l2.lon) 0
    else if(l1.lat == -l2.lat && l1.lon == -l2.lon) Pi * 6371
    val theta = acos((sin(toRadians(l1.lat)) * sin(toRadians(l2.lat))) +
      (cos(toRadians(l1.lat)) * cos(toRadians(l2.lat)) *
        cos(abs(toRadians(l1.lon) - toRadians(l2.lon)))))
    theta * 6371
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
//    val distList = temperatures.par.map(x => (x._2, computeDistance(x._1, location)))
//    if(distList.exists(x => x._2 <= 1)) distList.find(x => x._2 <=1).get._1
//    else  {
//      val p = 2
//      val temp = distList.par.map(x => (x._1, 1 / pow(x._2, p))).par.map(x => (x._1 * x._2, x._2)).par.reduce((a, b) => (a._1 + b._1, a._2 + b._2))
////      println(temp._1 + "," + temp._2)
//      temp._1 / temp._2
//    }
    val p = 2
    def recIdw(values: Iterator[(Location, Double)], sumVals: Double, sumWeights: Double): Double = {
      values.next match {
        case (xi, ui) => {
          val arc_distance = computeDistance(location, xi)
          if (arc_distance <= 1)
            ui
          else {
            val w = 1.0 / pow(arc_distance, p)
            if (values.hasNext) recIdw(values, sumVals + w * ui, sumWeights + w)
            else (sumVals + w * ui) / (sumWeights + w)
          }
        }
      }
    }
    recIdw(temperatures.toIterator, 0.0, 0.0)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    def interpolate(before: (Temperature, Color), after: (Temperature, Color), cur: Temperature): Color = {
      val scale = (cur - before._1) / (after._1 - before._1)
      val r = (after._2.red - before._2.red) * scale + before._2.red
      val b = (after._2.blue - before._2.blue) * scale + before._2.blue
      val g = (after._2.green - before._2.green) * scale + before._2.green
      Color((r + 0.5).toInt, (g + 0.5).toInt ,(b + 0.5).toInt)
    }
    // find the right place to interpolate the current point
    def findPos(last: (Temperature, Color), rest: Iterable[(Temperature, Color)]): Color = {
      rest match {
        case hd :: tl =>
          if(hd._1 < value) {
            if(last._1 == INIT_TEMP) hd._2
            else interpolate(hd, last, value)
          }
          else findPos(hd, tl)
        case Nil => last._2
      }
    }
    // sort to descending order
    findPos((INIT_TEMP, Color(0, 0, 0)), points.toSeq.sortBy(-_._1))
  }
  def generateLocation(x: Double, y: Double): Location = Location(90 - y, x - 180)


  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    // a helper function to generate location from the generator
    val width = 360
    val height = 180
    val pixels = new Array[Pixel](width * height)
    for(x <- 0 until width) {
      for(y <- 0 until height) {
        val temperature = predictTemperature(temperatures, generateLocation(x, y))
        val col = interpolateColor(colors, temperature)
//        println(temperature)
        // create pixel from the predicted temperature
        pixels(y * width + x) = Pixel(col.red, col.green, col.blue, 127)
      }
    }
    val image = Image(width, height, pixels)
    image.output(new java.io.File("target/some-image.png"))
    image
  }

}

