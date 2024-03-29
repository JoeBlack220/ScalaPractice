package observatory
import Visualization._
/**
  * 4th milestone: value-added information
  */
object Manipulation {
  def gridToLocation(grid: GridLocation) : Location = Location(grid.lat.toDouble, grid.lon.toDouble)
  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    g => predictTemperature(temperatures, gridToLocation(g))
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    g => temperaturess.map(x => makeGrid(x)).map(_(g)).sum / temperaturess.size
  }
  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    g => makeGrid(temperatures)(g) - normals(g)
  }


}

