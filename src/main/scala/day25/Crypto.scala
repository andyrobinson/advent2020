package day25

object Crypto {
  def findEncryptionKey(pk1: Long, pk2: Long): Long =  {
    val loopSize = findLoopSize(pk1)
    calculateKey(pk2,loopSize)
  }

  def findLoopSize(searchKey: Long, currentValue: Long = 1, subjectNumber: Long = 7, loopSize: Long = 1):Long = {
    val newValue = doTheHash(currentValue, subjectNumber)
    if (newValue == searchKey) loopSize
    else findLoopSize(searchKey, newValue, subjectNumber, loopSize + 1)

  }

  private def doTheHash(currentValue: Long, subjectNumber: Long) = {
    (currentValue * subjectNumber) % 20201227
  }

  def calculateKey(subjectNumber: Long, iterations: Long, currentValue: Long = 1): Long = {
    if (iterations == 0) currentValue
    else calculateKey(subjectNumber, iterations-1, doTheHash(currentValue, subjectNumber))
  }
}
