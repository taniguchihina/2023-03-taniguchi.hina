object LEA128 {
  private val delta = Array(0xc3efe9db, 0x44626b02, 0x79e27c8a, 0x78df30ec,0x715ea49e, 0xc785da0a, 0xe04ef22a, 0xe5c40957)
  
  def rotl(a: Int, s: Int): Int = {
    (a << s) | (a >>> (32-s))
  }
  
  def rotr(a: Int, s: Int): Int = {
    (a >>> s) | (a << (32-s))
  }
  
  def generateRoundKeys(key: Array[Int]): Array[Int] = {
    var roundKeys = new Array[Int](24 * 6)
    var t = Array(key(0), key(1), key(2), key(3))
    for (i <- 0 until 24) {
      t(0) = rotl(t(0) + rotl(delta(i % 4), i+0),  1)
      t(1) = rotl(t(1) + rotl(delta(i % 4), i+1),  3)
      t(2) = rotl(t(2) + rotl(delta(i % 4), i+2),  6)
      t(3) = rotl(t(3) + rotl(delta(i % 4), i+3), 11)
      roundKeys(6*i+0) = t(0)
      roundKeys(6*i+1) = t(1)
      roundKeys(6*i+2) = t(2)
      roundKeys(6*i+3) = t(1)
      roundKeys(6*i+4) = t(3)
      roundKeys(6*i+5) = t(1)
    }
    return roundKeys
  }
  
  def encrypt(plaintext: Array[Int], key: Array[Int]): Array[Int] = {
    val roundKeys = generateRoundKeys(key)
    var x = new Array[Int](4)
    plaintext.copyToArray(x)
    var x_next = Array(0, 0, 0, 0)
    for (i <- 0 until 24) {
      x_next(0) = rotl((x(0) ^ roundKeys(6*i+0)) + (x(1) ^ roundKeys(6*i+1)), 9)
      x_next(1) = rotr((x(1) ^ roundKeys(6*i+2)) + (x(2) ^ roundKeys(6*i+3)), 5)
      x_next(2) = rotr((x(2) ^ roundKeys(6*i+4)) + (x(3) ^ roundKeys(6*i+5)), 3)
      x_next(3) = x(0)
      x_next.copyToArray(x)
    }
    return x_next
  }
  
  def decrypt(ciphertext: Array[Int], key: Array[Int]): Array[Int] = {
    val roundKeys = generateRoundKeys(key)
    var x = new Array[Int](4)
    ciphertext.copyToArray(x)
    var x_next = Array(0, 0, 0, 0)
    for (i <- 0 until 24) {
      x_next(0) = x(3)
      x_next(1) = (rotr(x(0), 9) - (x_next(0) ^ roundKeys(6*(24-i-1)+0))) ^ roundKeys(6*(24-i-1)+1)
      x_next(2) = (rotl(x(1), 5) - (x_next(1) ^ roundKeys(6*(24-i-1)+2))) ^ roundKeys(6*(24-i-1)+3)
      x_next(3) = (rotl(x(2), 3) - (x_next(2) ^ roundKeys(6*(24-i-1)+4))) ^ roundKeys(6*(24-i-1)+5)
      x_next.copyToArray(x)
    }
    return x
  }

 def main(args: Array[String]): Unit = {
       
    val key = Array(0x3c2d1e0f, 0x78695a4b, 0xb4a59687, 0xf0e1d2c3)
    val plaintext = Array(0x13121110, 0x17161514, 0x1b1a1918, 0x1f1e1d1c)
    val ciphertext = Array(0x354ec89f, 0x18c6c628, 0xa7c73255, 0xfd8b6404)

    val start = System.currentTimeMillis
    for(i <- 0 to 2500000){
      val  c=encrypt(plaintext,key)
    }
    val end = System.currentTimeMillis

    println(end-start)

    val start2 = System.currentTimeMillis
    for(i <- 0 to 2500000){
      val  c=decrypt(plaintext,key)
    }
    val end2 = System.currentTimeMillis

    println(end2-start2)

    }

}
