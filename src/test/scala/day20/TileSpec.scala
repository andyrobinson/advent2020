package day20

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class TileSpec extends AnyFunSpec with Matchers {
  it("should read a tile from lines of input") {
    val lines = List("Tile 2311:",
      "..##.#..#.",
      "##..#.....",
      "#...##..#.",
      "####.#...#",
      "##.##.###.",
      "##...#.###",
      ".#.#.#..##",
      "..#....#..",
      "###...#.#.",
      "..###..###")
    val tile = Tile.fromLines(lines)

    tile shouldBe Tile(2311,List("..##.#..#.","...#.##..#","###..###..",".#..#####."))
  }

  it("should rotate a tile a given number of places") {
    val tile = Tile(10,List("top", "right", "bottom", "left"))
    Tile.rotateLeft(tile,1) shouldBe Tile(10,List("right","bottom","left","top"))
    Tile.rotateLeft(tile,2) shouldBe Tile(10,List("bottom","left","top","right"))
    Tile.rotateLeft(tile,3) shouldBe Tile(10,List("left","top","right","bottom"))
    Tile.rotateLeft(tile,4) shouldBe tile
  }

  it("should flip a tile") {
    val tile = Tile(20,List("top", "right", "bottom", "left"))
    Tile.flipHoriz(tile) shouldBe Tile(20,List("pot","tfel","mottob","thgir"))
  }

  it("should produce all permutations (there should be 8)") {
    val tile = Tile(20,List("top", "right", "bottom", "left"))
    Tile.permutations(tile).size shouldBe 8
  }

  it("should check if fits on right") {
    val tile1 = Tile(1,List("###.", ".#.#", "bottom", "left"))
    val tile2 = Tile(2,List("top", "right", "bottom", "#.#."))
    val tile3 = Tile(3,List("top", "right", "bottom", ".#.#"))
    tile1.acceptsRight(tile2) shouldBe true
    tile1.acceptsRight(tile3) shouldBe false
  }

  it("should check if it fits underneath") {
    val tile1 = Tile(1,List("top", "right", "##..", "left"))
    val tile2 = Tile(2,List("##..", "right", "bottom", "left"))
    val tile3 = Tile(3,List("..##", "right", "bottom", "left"))
    tile1.acceptsBottom(tile2) shouldBe false
    tile1.acceptsBottom(tile3) shouldBe true

  }
}
