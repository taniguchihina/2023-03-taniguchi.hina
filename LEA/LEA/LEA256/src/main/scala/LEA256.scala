object LEA256 {
  private val delta = Array(0xc3efe9db, 0x44626b02, 0x79e27c8a, 0x78df30ec,0x715ea49e, 0xc785da0a, 0xe04ef22a, 0xe5c40957)
  
  def rotl(a: Int, s: Int): Int = {
    (a << s) | (a >>> (32-s))
  }
  
  def rotr(a: Int, s: Int): Int = {
    (a >>> s) | (a << (32-s))
  }
  
  def generateRoundKeys(key: Array[Int]): Array[Int] = {
    var roundKeys = new Array[Int](32 * 6)
    var t = Array(key(0), key(1), key(2), key(3),key(4),key(5),key(6),key(7))
    for (i <- 0 until 32) {
      t((6*i+0) % 8) = rotl(t((6*i+0) % 8) + rotl(delta(i % 8), i+0),  1)
      t((6*i+1) % 8) = rotl(t((6*i+1) % 8) + rotl(delta(i % 8), i+1),  3)
      t((6*i+2) % 8) = rotl(t((6*i+2) % 8) + rotl(delta(i % 8), i+2),  6)
      t((6*i+3) % 8) = rotl(t((6*i+3) % 8) + rotl(delta(i % 8), i+3), 11)
      t((6*i+4) % 8) = rotl(t((6*i+4) % 8) + rotl(delta(i % 8), i+4), 13)
      t((6*i+5) % 8) = rotl(t((6*i+5) % 8) + rotl(delta(i % 8), i+5), 17)
      roundKeys(6*i+0) = t((6*i+0) % 8)
      roundKeys(6*i+1) = t((6*i+1) % 8)
      roundKeys(6*i+2) = t((6*i+2) % 8)
      roundKeys(6*i+3) = t((6*i+3) % 8)
      roundKeys(6*i+4) = t((6*i+4) % 8)
      roundKeys(6*i+5) = t((6*i+5) % 8)
    }
    return roundKeys
  }
    
  def encrypt(plaintext: Array[Int], key: Array[Int]): Array[Int] = {
    val roundKeys = generateRoundKeys(key)
    var x = new Array[Int](4)
    plaintext.copyToArray(x)
    var x_next = Array(0, 0, 0, 0)
    for (i <- 0 until 32) {
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
    for (i <- 0 until 32) {
      x_next(0) = x(3)
      x_next(1) = (rotr(x(0), 9) - (x_next(0) ^ roundKeys(6*(32-i-1)+0))) ^ roundKeys(6*(32-i-1)+1)
      x_next(2) = (rotl(x(1), 5) - (x_next(1) ^ roundKeys(6*(32-i-1)+2))) ^ roundKeys(6*(32-i-1)+3)
      x_next(3) = (rotl(x(2), 3) - (x_next(2) ^ roundKeys(6*(32-i-1)+4))) ^ roundKeys(6*(32-i-1)+5)
      x_next.copyToArray(x)
    }
    return x
  }

  def main(args: Array[String]): Unit = {
       
    val key = Array(0x3c2d1e0f, 0x78695a4b, 0xb4a59687, 0xf0e1d2c3, 0xc3d2e1f0, 0x8796a5b4, 0x4b5a6978, 0x0f1e2d3c)
    val plaintext = Array(0x33323130, 0x37363534, 0x3b3a3938, 0x3f3e3d3c)
    val ciphertext = Array(0xf6af51d6, 0xc189b147, 0xca00893a, 0x97e1f927)

    val start = System.currentTimeMillis
    for(i <- 0 to 1700000){
      val  c=encrypt(plaintext,key)
    }
    val end = System.currentTimeMillis

    println(end-start)

    val start2 = System.currentTimeMillis
    for(i <- 0 to 1700000){
      val  c=decrypt(plaintext,key)
    }
    val end2 = System.currentTimeMillis

    println(end2-start2)

    }

}
