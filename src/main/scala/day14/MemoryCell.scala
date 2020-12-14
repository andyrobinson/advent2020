package day14

case class MemoryCell(value: Long, mask: String) {
  def maskedValue: Long = {
    val andMask = BigInt(mask.replaceAll("X","1"),2).toLong
    val orMask = BigInt(mask.replaceAll("X","0"),2).toLong
    (value & andMask) | orMask
  }
}

object MemoryCell {
  def maskedAddresses(address: Long, mask: String): Set[Long]= {
    val addressAsBits = to36bitString(address)
    val zipped = mask.zip(addressAsBits).toList
    genAddresses(zipped,"")
  }

  private def genAddresses(maskAndAddress: List[(Char,Char)], inProgress: String): Set[Long] = {
    maskAndAddress match {
      case Nil => Set(BigInt(inProgress,2).toLong)
      case hd::tl => {
        hd._1 match {
          case '0' => genAddresses(tl, inProgress + hd._2)
          case '1' => genAddresses(tl, inProgress + "1")
          case 'X' => genAddresses(tl, inProgress + "1") ++ genAddresses(tl, inProgress + "0")
          case _ => throw new RuntimeException ("unexpected character in mask")
        }
      }
    }
  }

  private def to36bitString(l:Long): String = {
    String.format("%36s", l.toBinaryString).replaceAll(" ","0")
  }
}