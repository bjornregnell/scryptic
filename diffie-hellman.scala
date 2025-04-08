//> using scala 3.7.0-RC1
//> using toolkit 0.7.0

object DiffieHellman:
  val RNG = java.security.SecureRandom()
  val G = BigInt(2)  // used as base in modPow, should be a primitive root modulo P
  val DefaultBitLength = 4096

  def probablePrime(bitLength: Int = DefaultBitLength): BigInt = BigInt.probablePrime(bitLength, RNG)

  def prime(bitLength: Int = DefaultBitLength): BigInt =
    var p: BigInt = null
    while 
      p = probablePrime(bitLength) // a safe prime
      !p.isProbablePrime(certainty = bitLength)  // check that it is really probably a prime
    do ()
    p


  def keyPair(prime: BigInt, bitLength: Int = DefaultBitLength): (publicKey: BigInt, privateKey: BigInt) =
    val secret = BigInt(bitLength, RNG)
    (G.modPow(secret, prime), secret) 

  def sharedSecret(otherPublicKey: BigInt, mySecretKey: BigInt, prime: BigInt): BigInt =
    otherPublicKey.modPow(mySecretKey, prime)


@main def testDH = 
  val nbrTests = 5
  println(s"Generating big prime of bit length ${DiffieHellman.DefaultBitLength}")
  val P = DiffieHellman.prime()

  for i <- 1 to nbrTests do
    println(s"\n  *** Test run number $i")
    val t0 = System.currentTimeMillis()

    val serverKeys = DiffieHellman.keyPair(P)
    println(s"Send server public key to client: ${serverKeys.publicKey}")
    
    val clientKeys = DiffieHellman.keyPair(P)
    println(s"Send client public key to server: ${clientKeys.publicKey}")
    
    val serverSharedSecret = DiffieHellman.sharedSecret(clientKeys.publicKey, serverKeys.privateKey, P)
    val clientSharedSecret = DiffieHellman.sharedSecret(serverKeys.publicKey, clientKeys.privateKey, P)

    println(s"computed secret by Server: serverSharedSecret=$serverSharedSecret")
    println(s"computed secret by Client: clientSharedSecret=$clientSharedSecret")
    println(s"serverSharedSecret == clientShared ${serverSharedSecret == clientSharedSecret}")
    assert(serverSharedSecret == clientSharedSecret)
    println(s"cpu time spent on both server and client: ${System.currentTimeMillis() - t0} ms")
    println(s"big prime is $P")


  
