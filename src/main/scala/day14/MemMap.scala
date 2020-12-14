package day14

case class MemMap(cells: Map[Long, MemoryCell]) {
  def maskedSum :Long = {
    cells.values.foldLeft(0L)((acc,cell) => acc + cell.maskedValue)
  }

  def plainSum :Long = {
    cells.values.foldLeft(0L)((acc,cell) => acc + cell.value)
  }

}

object MemMap {
  val maskRegEx = """^mask\s=\s([1|0|X]+)$""".r
  val cellRegEx = """^mem\[([0-9]+)\]\s=\s([0-9]+)$""".r

  def fromLines(input: List[String]):MemMap = {
    MemMap(readLines(input, Map.empty[Long, MemoryCell], ""))
  }

  private def readLines(lines: List[String], cells: Map[Long, MemoryCell], mask: String): Map[Long, MemoryCell] = {
    lines match {
      case Nil => cells
      case hd :: tl => hd match {
        case maskRegEx(newMask) => readLines(tl, cells, newMask)
        case cellRegEx(address, value) => readLines(tl, cells + (address.toLong -> MemoryCell(value.toLong, mask)),mask)
        case _ => throw new RuntimeException("unrecognised input line: " + hd)
      }
    }
  }

  def AddressDecode(input: List[String]):MemMap =
    MemMap(decodeLines(input, Map.empty[Long, MemoryCell], ""))

  private def decodeLines(lines: List[String], cells: Map[Long, MemoryCell], mask: String): Map[Long, MemoryCell] = {
    lines match {
      case Nil => cells
      case hd :: tl => hd match {
        case maskRegEx(newMask) => decodeLines(tl, cells, newMask)
        case cellRegEx(address, value) => {
          val addresses = MemoryCell.maskedAddresses(address.toLong, mask)
          val additionalEntries = addresses.map(_ -> MemoryCell(value.toLong, mask))
          decodeLines(tl, cells ++ additionalEntries, mask)
        }
        case _ => throw new RuntimeException("unrecognised input line: " + hd)
      }
    }
  }

}