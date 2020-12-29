package day25

object Main extends App {
  val cardPublicKey = 2069194L
  val doorPublicKey = 16426071L

  val key = Crypto.findEncryptionKey(cardPublicKey,doorPublicKey)

  println("Answer1: " + key)
}
