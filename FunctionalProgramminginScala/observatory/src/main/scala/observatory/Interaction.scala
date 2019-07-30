package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math._
import Visualization._
/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    tileLocationCompute(tile.x.toDouble, tile.y.toDouble, tile.zoom.toDouble)
  }
  def tileLocationCompute(x: Double, y: Double, zoom: Double): Location = {
    val n = pow(2.0, zoom)
    val lon = ((x.toDouble / n) % 1.0) * 360.0 - 180.0
    val lat = ((atan(sinh(Pi * (1.0 - 2.0 * y / n))).toDegrees + 90) % 180.0) - 90
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], tile: Tile): Image = {
    val imageWidth = 256
    val imageHeight = 256
    val alpha = 127

    val pixels = (0 until imageWidth * imageHeight)
      .par.map(pos => {
      val xPos = (pos % imageWidth).toDouble / imageWidth + tile.x // column of image as fraction with offset x
      val yPos = (pos / imageHeight).toDouble / imageHeight + tile.y // row of image as fraction with offset y

      val col = interpolateColor(
        colors,
        predictTemperature(
          temperatures,
          tileLocationCompute(xPos, yPos, tile.zoom)
        )
      )
      Pixel(col.red, col.green, col.blue, alpha)
    })


    Image(imageWidth, imageHeight, pixels.toArray)
  }


  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      (year, data) <- yearlyData
      zoom <- 0 to 3
      x <- 0 until 1 << zoom
      y <- 0 until 1 << zoom
    } {
      generateImage(year, Tile(x, y, zoom), data)
    }
  }


}
