// Arithmetic operations in LEA

class Arith {
  def xor(a: Int, b: Int): Int = {
    a ^ b
  }
  
  def rotl(a: Int, s: Int): Int = {
    (a << s) | (a >>> (32-s))
  }
  
  def rotr(a: Int, s: Int): Int = {
    (a >>> s) | (a << (32-s))
  }
  
  def usadd(a: Int, b: Int): Int = {
    a + b
  }
  
  def ussub(a: Int, b: Int): Int = {
    a - b
  }
}

// end of file
