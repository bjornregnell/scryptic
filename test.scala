// run with scala run .
import scryptic.* 

enum TestCase:
  case DiffieHellman, AES

val verbose = false
def dbg(s: String): Unit = if verbose then println(s)

val runTheseTestCases = Set(TestCase.AES)
@main def test = 
  if runTheseTestCases.contains(TestCase.DiffieHellman) then 

    println(s"Generating big prime of bit length ${DiffieHellman.DefaultBitLength}...")
    var t0 = System.currentTimeMillis()
    val P = DiffieHellman.probablePrime()
    println(s"   ... it took ${System.currentTimeMillis() - t0} ms")

    val nbrTests = 10
    for i <- 1 to nbrTests do
      dbg(s"\n  *** Test run number $i")
      t0 = System.currentTimeMillis()

      val serverKeys = DiffieHellman.keyPair(P)
      dbg(s"Send server public key to client: ${serverKeys.publicKey}")
      
      val clientKeys = DiffieHellman.keyPair(P)
      dbg(s"Send client public key to server: ${clientKeys.publicKey}")
      
      val serverSharedSecret = DiffieHellman.sharedSecret(clientKeys.publicKey, serverKeys.privateKey, P)
      val clientSharedSecret = DiffieHellman.sharedSecret(serverKeys.publicKey, clientKeys.privateKey, P)

      dbg(s"computed secret by Server: serverSharedSecret=$serverSharedSecret")
      dbg(s"computed secret by Client: clientSharedSecret=$clientSharedSecret")
      dbg(s"serverSharedSecret == clientShared ${serverSharedSecret == clientSharedSecret}")
      assert(serverSharedSecret == clientSharedSecret)
      println(s"cpu time spent on both server and client: ${System.currentTimeMillis() - t0} ms")
      dbg(s"big prime is $P")
    
    else if runTheseTestCases.contains(TestCase.AES) then 
      case class Id(s: String)
      case class Gurka(id: Id, w: Int)
      val enc = AES.encryptObjectToString(Gurka(Id("id"),10), "hejsan") 
      val dec = AES.decryptObjectFromString[Gurka](enc, "hejsan")
      println(enc)
      println(dec)
 