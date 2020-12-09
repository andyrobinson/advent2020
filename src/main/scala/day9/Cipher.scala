package day9

object Cipher {
  def findRange(sum: Long, toSearch: List[Long]): List[Long] =
    toSearch match {
      case _::tl => searchForSum(sum, toSearch).getOrElse(findRange(sum, tl))
      case _ => throw new RuntimeException("no sum")
    }

  private def searchForSum(target: Long, toSum: List[Long]): Option[List[Long]] = {
    toSum match {
      case hd::_ if (target == hd) => Some(hd::Nil)
      case hd::tl if (target > hd) => searchForSum(target-hd, tl).map(hd::_)
      case _ => None
    }
  }

  def findInvalid(preamble: List[Long], factors: List[Long]): Long = factors match {
    case hd::tl => if (!containsSum(hd, preamble)) hd else findInvalid(preamble.tail :+ hd, tl)
    case Nil => throw new RuntimeException("No invalid number found")
  }

  def containsSum(sum: Long, parts: List[Long]): Boolean =
    parts match {
      case hd::tl => tl.exists(_ + hd == sum) || containsSum(sum,tl)
      case Nil => false
    }
}
