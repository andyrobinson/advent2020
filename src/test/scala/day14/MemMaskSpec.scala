package day14

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MemMaskSpec extends AnyFunSpec with Matchers {
  it ("should read the input into a memory map with overwriting") {
    val input = List("mask = 11X00XX1010X", "mem[10] = 4155", "mem[20] = 494929", "mask = 11010X0X00101", "mem[20] = 73599", "mem[30] = 97865")
    val memMap = MemMap.fromLines(input)
    memMap.cells shouldBe Map(10->MemoryCell(4155L, "11X00XX1010X"), 20->MemoryCell(73599L, "11010X0X00101"), 30->MemoryCell(97865L, "11010X0X00101"))
  }

  it("should use the mask to calculate the masked value") {
    MemoryCell(8L, "1XXXX").maskedValue shouldBe 24L
    MemoryCell(8L, "X0XX1").maskedValue shouldBe 1L
    MemoryCell(0xFFL, "1XX110X1").maskedValue shouldBe 0xFBL
    MemoryCell(0L, "1XX110X1").maskedValue shouldBe 0x99L
  }

  it("should work with 36 bit numbers") {
    MemoryCell(0L, "1XXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX".replaceAll(" ","")).maskedValue shouldBe scala.math.pow(2L,35L)
    MemoryCell(0xFFFFFFFFFL, "01XX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX".replaceAll(" ","")).maskedValue shouldBe scala.math.pow(2L,35L) - 1
    MemoryCell(0xFFFFFFFFFL, "0100 00000000 00000000 00000000 00000000".replaceAll(" ","")).maskedValue shouldBe scala.math.pow(2L,34L)
  }

  it("should sum the memory cells") {
    val memMap = MemMap.fromLines(List("mask = 00XX1100", "mem[10] = 255", "mem[20] = 494929", "mask = 01XXXXXXX11", "mem[20] = 512", "mem[30] = 1029"))
    memMap.maskedSum shouldBe 60 + 515 + 519
  }

  it("should produce a list of masked addresses") {
    MemoryCell.maskedAddresses(0x99L, "00000000000000000000000000000000001X") shouldBe Set(0x9AL,0x9BL)
    MemoryCell.maskedAddresses(0xFFFFFFFFFL, "010000000000000000000000000X00000000") shouldBe Set(68719476735L, 68719476479L)
    MemoryCell.maskedAddresses(26, "00000000000000000000000000000000X0XX") shouldBe Set(16L,17L,18L,19L,24L,25L,26L,27L)
  }

  it("should create a new memory map with the extended values in") {
    val memMap = MemMap.AddressDecode(List("mask = 000000000000000000000000000000X1001X","mem[42] = 100","mask = 00000000000000000000000000000000X0XX","mem[26] = 1"))
    memMap.plainSum shouldBe 208
  }
}






