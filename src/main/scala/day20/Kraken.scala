package day20

object Kraken {
  //                  #
  //#    ##    ##    ###
  // #  #  #  #  #  #
  private val coords: List[(Int,Int)] = List ((18,0),(0,1),(5,1),(6,1),(11,1),(12,1),(17,1),(18,1),(19,1),(1,2),(4,2),(7,2),(10,2),(13,2),(16,2))

  def hashes:Int = coords.size

  def isAt(point: (Int,Int), image: List[String]): Boolean = {
    coords.forall{ case(x,y) => image(y + point._2)(x + point._1) == '#' }
  }
}
