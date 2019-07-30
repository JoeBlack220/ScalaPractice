package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Visualization._
import observatory.Interaction.tileLocationCompute
import scala.math._
/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    d00 * (1 - point.x) * (1 - point.y) + d10 * point.x * (1 - point.y) + d01 * (1 - point.x) * point.y + d11 * point.x * point.y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    val imageWidth = 256
    val imageHeight = 256
    val alpha = 127
    val pixels = (0 until imageWidth * imageHeight)
      .par.map(pos => {
      val xPos = (pos % imageWidth).toDouble / imageWidth + tile.x // column of image as fraction with offset x
      val yPos = (pos / imageHeight).toDouble / imageHeight + tile.y // row of image as fraction with offset y
      val location = tileLocationCompute(xPos, yPos, tile.zoom)
      val latCeil = location.lat.ceil.toInt
      val latFloor = location.lat.floor.toInt
      val lonCeil = location.lon.ceil.toInt
      val lonFloor = location.lon.floor.toInt
      val d00 = grid(GridLocation(latFloor, lonFloor))
      val d01 = grid(GridLocation(latCeil, lonFloor))
      val d10 = grid(GridLocation(latFloor, lonCeil))
      val d11 = grid(GridLocation(latCeil, lonCeil))
      val temp = bilinearInterpolation(CellPoint((location.lon - lonFloor), (location.lat - latFloor))
        , d00, d01, d10, d11)
      val col = interpolateColor(colors, temp)
      Pixel(col.red, col.green, col.blue, alpha)
    })

    Image(imageWidth, imageHeight, pixels.toArray)
  }

}
