
// tle
object Solution {
  def change(amount: Int, coins: Array[Int]): Int = {
    val dp = new scala.collection.mutable.HashMap[String, Int]()
    def helper(money: Int, index : Int) : Int = {
      val sig = money + "," + index
      if(dp.contains(sig)) {
        dp.get(sig) match {
          case Some(u) => u
        }
      }
      else if (money == 0) {
        dp.put(sig, 1)
        1
      }
      else if (money < 0 || index > coins.length - 1) {
        dp.put(sig, 0)
        0
      }
      else if (money <= 0 && index < coins.length) {
        dp.put(sig, 0)
        0
      }
      else {
        val sum = helper(money, index + 1) + helper(money - coins(index), index)
        dp.put(sig, sum)
        sum
      }
    }
    helper(amount, 0)
  }
}