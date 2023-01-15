import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams

class LEA256Spec extends AnyFlatSpec with Diagrams {

  val lea256 = new LEA256
  
  "rotl" should "8 tests" in {
    // left rotation
    assert(lea256.rotl(0x5f66dbf6,  4) === 0xf66dbf65)
    assert(lea256.rotl(0xa0ec1b11, 16) === 0x1b11a0ec)
    assert(lea256.rotl(0x7fc728e2, 10) === 0x1ca389ff)
    assert(lea256.rotl(0x9a35afec,  5) === 0x46b5fd93)
    assert(lea256.rotl(0x9a8da1c5,  7) === 0x46d0e2cd)
    assert(lea256.rotl(0x36367142, 12) === 0x67142363)
    assert(lea256.rotl(0x51685abf, 10) === 0xa16afd45)
    assert(lea256.rotl(0xcbd041bb, 22) === 0x6ef2f410)
  }
  
  "rotr" should "8 tests" in {
    // right rotation
    assert(lea256.rotr(0x95395fe6, 20) === 0x95fe6953)
    assert(lea256.rotr(0xf3b04aa5, 25) === 0xd82552f9)
    assert(lea256.rotr(0xb2817563, 30) === 0xca05d58e)
    assert(lea256.rotr(0xf79322b5, 20) === 0x322b5f79)
    assert(lea256.rotr(0xcecec4b5, 19) === 0xd896b9d9)
    assert(lea256.rotr(0xbc6fa4b8,  5) === 0xc5e37d25)
    assert(lea256.rotr(0x9e5bdeea, 10) === 0xbaa796f7)
    assert(lea256.rotr(0x69d6d22c, 11) === 0x458d3ada)
  }
  
  "generateRoundKeys" should "1 test" in {
    val key = Array(0x3c2d1e0f, 0x78695a4b, 0xb4a59687, 0xf0e1d2c3, 0xc3d2e1f0, 0x8796a5b4, 0x4b5a6978, 0x0f1e2d3c)
    val rk: Array[Int] = lea256.generateRoundKeys(key)
    assert(rk(  0) === 0x003a0fd4)
    assert(rk(  1) === 0x02497010)
    assert(rk(  2) === 0x194f7db1)
    assert(rk(  3) === 0x090d0883)
    assert(rk(  4) === 0x2ff5805a)
    assert(rk(  5) === 0xc2580b27)

    assert(rk(  6) === 0xa83e7ef9)
    assert(rk(  7) === 0x053eca29)
    assert(rk(  8) === 0xd359f988)
    assert(rk(  9) === 0x8101a243)
    assert(rk( 10) === 0x9bbf34b3)
    assert(rk( 11) === 0x9228434f)

    assert(rk( 12) === 0x2efee506)
    assert(rk( 13) === 0x8b5f7bd4)
    assert(rk( 14) === 0x9991e811)
    assert(rk( 15) === 0x72dbc20c)
    assert(rk( 16) === 0x2384c97f)
    assert(rk( 17) === 0xcefee47f)

    assert(rk( 18) === 0xc571782c)
    assert(rk( 19) === 0x00da90b1)
    assert(rk( 20) === 0xb940a552)
    assert(rk( 21) === 0x5db79619)
    assert(rk( 22) === 0x4bc9a125)
    assert(rk( 23) === 0x5d08a419)

    assert(rk( 24) === 0x72de26cc)
    assert(rk( 25) === 0xd69bc26f)
    assert(rk( 26) === 0x46a7f207)
    assert(rk( 27) === 0x66ff4d81)
    assert(rk( 28) === 0xa87862fc)
    assert(rk( 29) === 0xa5f63601)

    assert(rk( 30) === 0x7909c4fa)
    assert(rk( 31) === 0xf3f93651)
    assert(rk( 32) === 0x72cb0bcd)
    assert(rk( 33) === 0xae69b2e3)
    assert(rk( 34) === 0x80f2ca4b)
    assert(rk( 35) === 0xf13efcce)

    assert(rk( 36) === 0x7869db69)
    assert(rk( 37) === 0x6b7a5b8e)
    assert(rk( 38) === 0xfefbf6b1)
    assert(rk( 39) === 0xec608c8e)
    assert(rk( 40) === 0x76e9d5d2)
    assert(rk( 41) === 0x13ca4bf6)

    assert(rk( 42) === 0xc5eeec7a)
    assert(rk( 43) === 0xaa42a59d)
    assert(rk( 44) === 0x1f22cd00)
    assert(rk( 45) === 0xfdd92bdc)
    assert(rk( 46) === 0xd6bbe3e8)
    assert(rk( 47) === 0x15d459ec)

    assert(rk( 48) === 0xcda7632a)
    assert(rk( 49) === 0x9cf01bef)
    assert(rk( 50) === 0x6596e261)
    assert(rk( 51) === 0x8c1de14c)
    assert(rk( 52) === 0x1127c3b8)
    assert(rk( 53) === 0x48b3f629)

    assert(rk( 54) === 0x3723d0e1)
    assert(rk( 55) === 0xfc0317ec)
    assert(rk( 56) === 0x3fdd5378)
    assert(rk( 57) === 0x0201ae1d)
    assert(rk( 58) === 0xe55db65e)
    assert(rk( 59) === 0xe4c84dbc)

    assert(rk( 60) === 0x3633db3f)
    assert(rk( 61) === 0xe4c24fc2)
    assert(rk( 62) === 0xbb1e1fd7)
    assert(rk( 63) === 0xa339425c)
    assert(rk( 64) === 0xfe3e1bdf)
    assert(rk( 65) === 0xd61c808d)

    assert(rk( 66) === 0xbdca3449) 
    assert(rk( 67) === 0xbeb8aa4e)
    assert(rk( 68) === 0x145a9687)
    assert(rk( 69) === 0xeb6fcd87)
    assert(rk( 70) === 0x8b88ca72)
    assert(rk( 71) === 0x7677a84b)

    assert(rk( 72) === 0xd11005e9)
    assert(rk( 73) === 0x558275c5)
    assert(rk( 74) === 0xbc742819)
    assert(rk( 75) === 0x3f17e888)
    assert(rk( 76) === 0x20fcb71f)
    assert(rk( 77) === 0x60886959)

    assert(rk( 78) === 0x8d9446c4)
    assert(rk( 79) === 0x67d2d167)
    assert(rk( 80) === 0x855a6aef)
    assert(rk( 81) === 0x69ea517c)
    assert(rk( 82) === 0x36e48e11)
    assert(rk( 83) === 0x0d3f4e86)

    assert(rk( 84) === 0xbb0ede65)
    assert(rk( 85) === 0xcceecc06)
    assert(rk( 86) === 0xefc9c49f)
    assert(rk( 87) === 0x44902261)
    assert(rk( 88) === 0xbd8549c0)
    assert(rk( 89) === 0xa7e7f682)

    assert(rk( 90) === 0x772101e6)
    assert(rk( 91) === 0xb4b9a250)
    assert(rk( 92) === 0x6faa7b73)
    assert(rk( 93) === 0x7318b792)
    assert(rk( 94) === 0x1e57e751)
    assert(rk( 95) === 0xfd43b41c)

    assert(rk( 96) === 0x4ec21b5f)
    assert(rk( 97) === 0xdcfbf30b)
    assert(rk( 98) === 0xa4046947)
    assert(rk( 99) === 0xbe0e781c)
    assert(rk(100) === 0xd74e21ac)
    assert(rk(101) === 0x6b1f5d22)

    assert(rk(102) === 0xe8b8e02b)
    assert(rk(103) === 0x4a662d2d)
    assert(rk(104) === 0xb50f9ca9)
    assert(rk(105) === 0x01c98c69)
    assert(rk(106) === 0x9eb28089)
    assert(rk(107) === 0x216cfd3f)

    assert(rk(108) === 0x92f0126b)
    assert(rk(109) === 0x7b9961aa)
    assert(rk(110) === 0x581f94ac)
    assert(rk(111) === 0xab4be6dd)
    assert(rk(112) === 0xc2a91af5)
    assert(rk(113) === 0xfb4e8e0c)

    assert(rk(114) === 0x4c2c8f04)
    assert(rk(115) === 0x81a45991)
    assert(rk(116) === 0x1fcb946c)
    assert(rk(117) === 0xbccbb5b5)
    assert(rk(118) === 0x808899cb)
    assert(rk(119) === 0x8c1b2f89)

    assert(rk(120) === 0x192061be)
    assert(rk(121) === 0x78e5cf04)
    assert(rk(122) === 0xf239ab5c)
    assert(rk(123) === 0xe8471e86)
    assert(rk(124) === 0x9e6217c7)
    assert(rk(125) === 0xe5fdf35c)

    assert(rk(126) === 0x83c3150d)
    assert(rk(127) === 0x766887f8)
    assert(rk(128) === 0xa1092ac7)
    assert(rk(129) === 0x6aa6f41d)
    assert(rk(130) === 0x16e200f9)
    assert(rk(131) === 0x6bdc26ca)

    assert(rk(132) === 0x52345706)
    assert(rk(133) === 0xdb70d6af)
    assert(rk(134) === 0xa8d8ffeb)
    assert(rk(135) === 0x492ee661)
    assert(rk(136) === 0x4cd1e991)
    assert(rk(137) === 0xd75d8352)

    assert(rk(138) === 0x85a9c5fb)
    assert(rk(139) === 0x1e0f569e)
    assert(rk(140) === 0x7ff7c600)
    assert(rk(141) === 0x3f36a1d8)
    assert(rk(142) === 0xe406ad00)
    assert(rk(143) === 0x4ded8f16)

    assert(rk(144) === 0x512bb2f4)
    assert(rk(145) === 0x772b192c)
    assert(rk(146) === 0x2e6168bd)
    assert(rk(147) === 0x76af67e1)
    assert(rk(148) === 0xd893a786)
    assert(rk(149) === 0x3e276f69)

    assert(rk(150) === 0xd11ee3ad)
    assert(rk(151) === 0xb7f8c612)
    assert(rk(152) === 0xd3b19318)
    assert(rk(153) === 0x89fee4db)
    assert(rk(154) === 0xb6c3aedd)
    assert(rk(155) === 0x05420f90)

    assert(rk(156) === 0x04f662f0)
    assert(rk(157) === 0x8fb41a6c)
    assert(rk(158) === 0x2f42dd5e)
    assert(rk(159) === 0xa8ad1839)
    assert(rk(160) === 0x46474e45)
    assert(rk(161) === 0x46418de0)

    assert(rk(162) === 0x351550c8)
    assert(rk(163) === 0x668014f6)
    assert(rk(164) === 0x04924365)
    assert(rk(165) === 0x5f353d6f)
    assert(rk(166) === 0x4eba8d76)
    assert(rk(167) === 0x924a4318)

    assert(rk(168) === 0x5aba711c)
    assert(rk(169) === 0xa36b1398)
    assert(rk(170) === 0x5b3e7bf4)
    assert(rk(171) === 0x7b3a2cf9)
    assert(rk(172) === 0x1d006ebe)
    assert(rk(173) === 0x0d5683e5)

    assert(rk(174) === 0x4f56916f)
    assert(rk(175) === 0x215dccd2)
    assert(rk(176) === 0x9f57886f)
    assert(rk(177) === 0x876d1357)
    assert(rk(178) === 0x46013d49)
    assert(rk(179) === 0x2a4932a3)

    assert(rk(180) === 0xaa285691)
    assert(rk(181) === 0xebefe7d3)
    assert(rk(182) === 0xe960e64b)
    assert(rk(183) === 0xdd893f0f)
    assert(rk(184) === 0x6a234412)
    assert(rk(185) === 0x495d13c9)

    assert(rk(186) === 0x71c683e8)
    assert(rk(187) === 0x8069dfd0)
    assert(rk(188) === 0x6c1a501d)
    assert(rk(189) === 0x00699418)
    assert(rk(190) === 0x262142f0)
    assert(rk(191) === 0xa91a7393)
  }
  
  "encrypt" should "1 test" in {
    val key = Array(0x3c2d1e0f, 0x78695a4b, 0xb4a59687, 0xf0e1d2c3, 0xc3d2e1f0, 0x8796a5b4, 0x4b5a6978, 0x0f1e2d3c)
    val plaintext = Array(0x33323130, 0x37363534, 0x3b3a3938, 0x3f3e3d3c)
    val ciphertext = Array(0xf6af51d6, 0xc189b147, 0xca00893a, 0x97e1f927)
    val c = lea256.encrypt(plaintext, key)
    assert(c(0) === ciphertext(0))
    assert(c(1) === ciphertext(1))
    assert(c(2) === ciphertext(2))
    assert(c(3) === ciphertext(3))            
  }
  
  "decrypt" should "1 test" in {
    val key = Array(0x3c2d1e0f, 0x78695a4b, 0xb4a59687, 0xf0e1d2c3, 0xc3d2e1f0, 0x8796a5b4, 0x4b5a6978, 0x0f1e2d3c)
    val plaintext = Array(0x33323130, 0x37363534, 0x3b3a3938, 0x3f3e3d3c)
    val ciphertext = Array(0xf6af51d6, 0xc189b147, 0xca00893a, 0x97e1f927)
    val m = lea256.decrypt(ciphertext, key)
    assert(m(0) === plaintext(0))
    assert(m(1) === plaintext(1))
    assert(m(2) === plaintext(2))
    assert(m(3) === plaintext(3))            
  }
  
  
}

// end of file
