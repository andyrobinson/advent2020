package day24

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers



class HexTileSpec extends AnyFunSpec with Matchers {
  it("should read a line into a list of directions") {
    val input = "sesenwnenenewse"
    val directions: List[String] = HexTile.readline(input)
    directions shouldBe List("se","se","nw","ne","ne","ne","w","se")
  }

  it("should count a black tile") {
    val directions = HexTile.fromInput(List("nwnw","sese"))
    HexTile.blackTiles(directions) shouldBe Set((-2,2),(2,-2))
  }

  it("should flip a tile back when it already exists") {
    val directions = HexTile.fromInput(List("nwnw","sese","esesw"))
    HexTile.blackTiles(directions) shouldBe Set((-2,2))
  }

  it("should calculate the neighbours of a cell") {
    HexTile.neighbours((3,4)) shouldBe Set((2,4),(4,4),(2,5),(3,5),(3,3),(4,3))
  }

  it("should remove any black tiles with zero or more than 2 neighbours") {
    val blackTiles = Set((1,1),(10,10),(11,10),(10,11),(10,9))
    HexTile.surviving(blackTiles) shouldBe Set((11,10),(10,11),(10,9))
  }

  it("should generate new black tiles if there are exactly 2 neighbours") {
    val blackTiles = Set((11,10),(10,11))
    HexTile.flip(blackTiles) shouldBe Set((11,10),(10,11),(10,10),(11,11))
  }
}
