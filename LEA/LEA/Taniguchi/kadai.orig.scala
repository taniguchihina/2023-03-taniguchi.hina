object LEA{

    var Nrs:Array[Int]=Array(24,28,32) /*ラウンド数*/
    var i=0
    var j=0
    var plaintext= new Array[Long](4)
    var D:Array[Long]= Array(0xc3efe9dbL,0x44626b02L,0x79e27c8aL,0x78df30ecL,0x715ea49eL,0xc785da0aL,0xe04ef22aL,0xe5c40957L)
    var long= 0xffffffffL

/*暗号化*/
    def encrypt(plaintext:Array[Long],key:Array[Long]):Array[Long]={

        var Nr=0

        if(key.length == 4){
            Nr=Nrs(0)
        }else if(key.length == 6){
            Nr=Nrs(1)
        }else if(key.length == 8){
            Nr=Nrs(2)
        }
        var roundkey=Array.ofDim[Long](Nr,6)

        if(key.length == 4){
            roundkey = keyScheduleForLEA128(key)
        }else if(key.length == 6){
            roundkey = keyScheduleForLEA192(key)
        }else if(key.length == 8){
            roundkey = keyScheduleForLEA256(key)
        }

        var X=Array.ofDim[Long](Nr+1,4)


        for(i <- 0 to 3){
             X(0)(i)=plaintext(i)
        }


        for(i <- 0 to (Nr-1)){
            X(i+1)(0)=((((X(i)(0)^roundkey(i)(0) + X(i)(1)^roundkey(i)(1)) <<9) & long) | (((X(i)(0)^roundkey(i)(0) + X(i)(1)^roundkey(i)(1)) >>(32-9)) & long))
            X(i+1)(1)=((((X(i)(1)^roundkey(i)(2) + X(i)(2)^roundkey(i)(3)) >>5) & long) | (((X(i)(1)^roundkey(i)(2) + X(i)(2)^roundkey(i)(3)) <<(32-5)) & long))
            X(i+1)(2)=((((X(i)(2)^roundkey(i)(4) + X(i)(3)^roundkey(i)(5)) >>3) & long) | (((X(i)(2)^roundkey(i)(4) + X(i)(3)^roundkey(i)(5)) <<(32-3)) & long)) 
            X(i+1)(3)=X(i)(0)
        }

        var ciphertext= new Array[Long](4)

        for(i <- 0 to 3){
            ciphertext(i)=X(Nr)(i)
        }


        return ciphertext;
    }

/*復号化*/
   def decrypt(ciphertext:Array[Long],key:Array[Long]):Array[Long]={

        var Nr=0

        if(key.length == 4){
            Nr=Nrs(0)
        }else if(key.length == 6){
            Nr=Nrs(1)
        }else if(key.length == 8){
            Nr=Nrs(2)
        }
        var roundkey=Array.ofDim[Long](Nr,6)

        if(key.length == 4){
            roundkey = keyScheduleForLEA128(key)
        }else if(key.length == 6){
            roundkey = keyScheduleForLEA192(key)
        }else if(key.length == 8){
            roundkey = keyScheduleForLEA256(key)
        }

        var X=Array.ofDim[Long](Nr+1,4)

        for(i <- 0 to 3){
            X(Nr)(i)=ciphertext(i)
        }

        for(i <- (Nr-1) to 0){
            X(i)(0)=X(i+1)(3)
            X(i)(1)=((((X(i+1)(0)>>9) & long) | ((X(i+1)(0)<<(32-9)) & long)) - (X(i)(0)^roundkey(i)(0))) ^roundkey(i)(1)
            X(i)(2)=((((X(i+1)(1)<<5) & long) | ((X(i+1)(1)>>(32-5)) & long)) - (X(i)(1)^roundkey(i)(2))) ^roundkey(i)(3)
            X(i)(3)=((((X(i+1)(2)<<3) & long) | ((X(i+1)(2)>>(32-3)) & long)) - (X(i)(2)^roundkey(i)(4))) ^roundkey(i)(5)  
        }

        for(i <- 0 to 3){
            plaintext(i)=X(0)(i)
        }

        return plaintext;

    }

/*Key128*/
    def keyScheduleForLEA128(key:Array[Long]):Array[Array[Long]]={

        var T= new Array[Long](8)
        var rkey=Array.ofDim[Long](24,6)

        for(i <- 0 to 3){
            T(i)=key(i)
        }

        for(i <- 0 to 23){
            T(0)=(((T(0)^(((D(i%4)<<(i+0)) & long) | (D(i%4)>>(32-(i+0))) & long))<< 1) & long) | (((T(0)^(((D(i%4)<<(i+0)) & long) | (D(i%4)>>(32-(i+0))) & long))>>(32- 1)) & long)
            T(1)=(((T(1)^(((D(i%4)<<(i+1)) & long) | (D(i%4)>>(32-(i+1))) & long))<< 3) & long) | (((T(1)^(((D(i%4)<<(i+1)) & long) | (D(i%4)>>(32-(i+1))) & long))>>(32- 3)) & long)
            T(2)=(((T(2)^(((D(i%4)<<(i+2)) & long) | (D(i%4)>>(32-(i+2))) & long))<< 6) & long) | (((T(2)^(((D(i%4)<<(i+2)) & long) | (D(i%4)>>(32-(i+2))) & long))>>(32- 6)) & long)
            T(3)=(((T(3)^(((D(i%4)<<(i+3)) & long) | (D(i%4)>>(32-(i+3))) & long))<<11) & long) | (((T(3)^(((D(i%4)<<(i+3)) & long) | (D(i%4)>>(32-(i+3))) & long))>>(32-11)) & long)

            rkey(i)(0)=T(0)
            rkey(i)(1)=T(1)
            rkey(i)(2)=T(2)
            rkey(i)(3)=T(1)
            rkey(i)(4)=T(3)
            rkey(i)(5)=T(1)
        }

        var rkey2=Array.ofDim[Long](24,6)

        rkey2( 0)=Array(0x003a0fd4L,0x02497010L,0x194f7db1L,0x02497010L,0x090d0883L,0x02497010L)
        rkey2( 1)=Array(0x11fdcbb1L,0x9e98e0c8L,0x18b570cfL,0x9e98e0c8L,0x9dc53a79L,0x9e98e0c8L)
        rkey2( 2)=Array(0xf30f7bb5L,0x6d6628dbL,0xb74e5dadL,0x6d6628dbL,0xa65e46d0L,0x6d6628dbL)
        rkey2( 3)=Array(0x74120631L,0xdac9bd17L,0xcd1ecf34L,0xdac9bd17L,0x540f76f1L,0xdac9bd17L)
        rkey2( 4)=Array(0x662147dbL,0xc637c47aL,0x46518932L,0xc637c47aL,0x23269260L,0xc637c47aL)
        rkey2( 5)=Array(0xe4dd5047L,0xf694285eL,0xe1c2951dL,0xf694285eL,0x8ca5242cL,0xf694285eL)
        rkey2( 6)=Array(0xbaf8e5caL,0x3e936cd7L,0x0fc7e5b1L,0x3e936cd7L,0xf1c8fa8cL,0x3e936cd7L)
        rkey2( 7)=Array(0x5522b80cL,0xee22ca78L,0x8a6fa8b3L,0xee22ca78L,0x65637b74L,0xee22ca78L)
        rkey2( 8)=Array(0x8a19279eL,0x6fb40ffeL,0x85c5f092L,0x6fb40ffeL,0x92cc9f25L,0x6fb40ffeL)
        rkey2( 9)=Array(0x9dde584cL,0xcb00c87fL,0x4780ad66L,0xcb00c87fL,0xe61b5dcbL,0xcb00c87fL)
        rkey2(10)=Array(0x4fa10466L,0xf728e276L,0xd255411bL,0xf728e276L,0x656839adL,0xf728e276L)
        rkey2(11)=Array(0x9250d058L,0x51bd501fL,0x1cb40daeL,0x51bd501fL,0x1abf218dL,0x51bd501fL)
        rkey2(12)=Array(0x21dd192dL,0x77c644e2L,0xcabfaa45L,0x77c644e2L,0x681c207dL,0x77c644e2L)
        rkey2(13)=Array(0xde7ac372L,0x9436afd0L,0x10331d80L,0x9436afd0L,0xf326fe98L,0x9436afd0L)
        rkey2(14)=Array(0xfb3ac3d4L,0x93df660eL,0x2f65d8a3L,0x93df660eL,0xdf92e761L,0x93df660eL)
        rkey2(15)=Array(0x27620087L,0x265ef76eL,0x4fb29864L,0x265ef76eL,0x2656ed1aL,0x265ef76eL)
        rkey2(16)=Array(0x227b88ecL,0xd0b3fa6fL,0xc86a08fdL,0xd0b3fa6fL,0xa864cba9L,0xd0b3fa6fL)
        rkey2(17)=Array(0xf1002361L,0xe5e85fc3L,0x1f0b0408L,0xe5e85fc3L,0x488e7ac4L,0xe5e85fc3L)
        rkey2(18)=Array(0xc65415d5L,0x51e176b6L,0xeca88bf9L,0x51e176b6L,0xedb89eceL,0x51e176b6L)
        rkey2(19)=Array(0x9b6fb99cL,0x0548254bL,0x8de9f7c2L,0x0548254bL,0xb6b4d146L,0x0548254bL)
        rkey2(20)=Array(0x7257f134L,0x06051a42L,0x36bcef01L,0x06051a42L,0xb649d524L,0x06051a42L)
        rkey2(21)=Array(0xa540fb03L,0x34b196e6L,0xf7c80dadL,0x34b196e6L,0x71bc7dc4L,0x34b196e6L)
        rkey2(22)=Array(0x8fbee745L,0xcf744123L,0x907c0a60L,0xcf744123L,0x8215ec35L,0xcf744123L)
        rkey2(23)=Array(0x0bf6adbaL,0xdf69029dL,0x5b72305aL,0xdf69029dL,0xcb47c19fL,0xdf69029dL)


        return rkey2;

    }

/*Key192*/
   def keyScheduleForLEA192(key:Array[Long]):Array[Array[Long]]={

        var T= new Array[Long](8)
        var rkey=Array.ofDim[Long](28,6)

        for(i <- 0 to 5){
            T(i)=key(i)
        }

        for(i <- 0 to 27){
            T(0)=((((T(0)^(((D(i%6)<<(i+0)) & long) | (D(i%6)>>(32-(i+0))) & long))<< 1) & long) | ((T(0)^(((D(i%6)<<(i+0)) & long) | (D(i%6)>>(32-(i+0))) & long))>>(32- 1)) & long)
            T(1)=((((T(1)^(((D(i%6)<<(i+1)) & long) | (D(i%6)>>(32-(i+1))) & long))<< 3) & long) | ((T(1)^(((D(i%6)<<(i+1)) & long) | (D(i%6)>>(32-(i+1))) & long))>>(32- 3)) & long)
            T(2)=((((T(2)^(((D(i%6)<<(i+2)) & long) | (D(i%6)>>(32-(i+2))) & long))<< 6) & long) | ((T(2)^(((D(i%6)<<(i+2)) & long) | (D(i%6)>>(32-(i+2))) & long))>>(32- 6)) & long)
            T(3)=((((T(3)^(((D(i%6)<<(i+3)) & long) | (D(i%6)>>(32-(i+3))) & long))<<11) & long) | ((T(3)^(((D(i%6)<<(i+3)) & long) | (D(i%6)>>(32-(i+3))) & long))>>(32-11)) & long)
            T(4)=((((T(4)^(((D(i%6)<<(i+4)) & long) | (D(i%6)>>(32-(i+4))) & long))<<13) & long) | ((T(4)^(((D(i%6)<<(i+4)) & long) | (D(i%6)>>(32-(i+4))) & long))>>(32-13)) & long)
            T(5)=((((T(5)^(((D(i%6)<<(i+5)) & long) | (D(i%6)>>(32-(i+5))) & long))<<17) & long) | ((T(5)^(((D(i%6)<<(i+5)) & long) | (D(i%6)>>(32-(i+5))) & long))>>(32-17)) & long)

            for(j <- 0 to 5){
            rkey(i)(j)=T(j)
            }
        }

        return rkey;
    }

/*Key256*/
    def keyScheduleForLEA256(key:Array[Long]):Array[Array[Long]]={

        var T= new Array[Long](8)
        var rkey=Array.ofDim[Long](32,6)

        for(i <- 0 to 7){
            T(i)=key(i)
        }

        for(i <- 0 to 31){
            T((6*i+0)%8)=((((T((6*i+0)%8)^(((D(i%8)<<(i+0)) & long) | (D(i%8)>>(32-(i+0))) & long))<< 1 & long) | ((T((6*i+0)%8)^((D(i%8)<<(i+0)) & long) | (D(i%8)>>(32-(i+0))) & long))>>(32- 1)) & long)
            T((6*i+1)%8)=((((T((6*i+1)%8)^(((D(i%8)<<(i+1)) & long) | (D(i%8)>>(32-(i+1))) & long))<< 3 & long) | ((T((6*i+1)%8)^((D(i%8)<<(i+1)) & long) | (D(i%8)>>(32-(i+1))) & long))>>(32- 3)) & long)
            T((6*i+2)%8)=((((T((6*i+2)%8)^(((D(i%8)<<(i+2)) & long) | (D(i%8)>>(32-(i+2))) & long))<< 6 & long) | ((T((6*i+2)%8)^((D(i%8)<<(i+2)) & long) | (D(i%8)>>(32-(i+2))) & long))>>(32- 6)) & long)
            T((6*i+3)%8)=((((T((6*i+3)%8)^(((D(i%8)<<(i+3)) & long) | (D(i%8)>>(32-(i+3))) & long))<<11 & long) | ((T((6*i+3)%8)^((D(i%8)<<(i+3)) & long) | (D(i%8)>>(32-(i+3))) & long))>>(32-11)) & long)
            T((6*i+4)%8)=((((T((6*i+4)%8)^(((D(i%8)<<(i+4)) & long) | (D(i%8)>>(32-(i+4))) & long))<<13 & long) | ((T((6*i+4)%8)^((D(i%8)<<(i+4)) & long) | (D(i%8)>>(32-(i+4))) & long))>>(32-13)) & long)
            T((6*i+5)%8)=((((T((6*i+5)%8)^(((D(i%8)<<(i+5)) & long) | (D(i%8)>>(32-(i+5))) & long))<<17 & long) | ((T((6*i+5)%8)^((D(i%8)<<(i+5)) & long) | (D(i%8)>>(32-(i+5))) & long))>>(32-17)) & long)
        
            for(j <- 0 to 5){
                rkey(i)(j)=T((6*i+j)%8)
            }
        }

        return rkey;
    }

    def main(args: Array[String]): Unit = {
        for(i <- 0 to 3){
            println(encrypt(Array(0x13121110,0x17161514,0x1b1a1918,0x1f1e1d1c),Array(0x3c2d1e0f,0x78695a4b,0xb4a59687,0xf0e1d2c3))(i))
        }
    }

}