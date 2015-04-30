import org.scalacheck._
import org.scalacheck.Gen._
import Arbitrary.arbitrary

lazy val genMap: Gen[Map[Int,Int]] = for {
  k <- arbitrary[Int]
  v <- arbitrary[Int]
  m <- oneOf(const(Map.empty[Int,Int]), genMap)
} yield m.updated(k, v)

genMap.sample

val g = choose(-2, 5)
g.sample

val stringsGen = for {
  alpha <- Gen.alphaStr
  num <- Gen.numStr
  id <- Gen.identifier
} yield (alpha.take(4), num.take(4), id.take(4))

stringsGen.sample