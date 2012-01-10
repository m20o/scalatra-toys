package mm.scalatra.extension

import org.specs2.mutable.Specification
import org.scalatra.util.{MultiMap, MultiMapHeadView, MapWithIndifferentAccess}
import mm.scalatra.common.conversions.TypeConverter

class ParamsPimpingSpec extends Specification {

  case class FakeParams(params: Map[String, String]) extends MultiMapHeadView[String, String]
  with MapWithIndifferentAccess[String] {
    protected def multiMap = MultiMap(params.map(e => (e._1, List(e._2).toSeq)))

  }

  import Params._

  "Scalatra 'Params pimping'" should {

    "add a getAs[T] method to Scalatra Params that returns Option[T]" in {

      val params: ParamsType = FakeParams(Map("a" -> "1", "b" -> "", "c" -> null))

      params.getAs[Int]("a") must beSome
      params.getAs[Int]("a").get must_== 1

      params.getAs[Int]("b") must beNone
      params.getAs[Int]("c") must beNone

      params.getAs[Int]("unexistent") must beNone
    }

    "return None if a conversion is invalid" in {
      val params: ParamsType = FakeParams(Map("a" -> "hello world"))
      params.getAs[Int]("a") must beNone
    }

    "implicitly find TypeConverter(s) for a custom type" in {

      case class Bogus(name: String)

      implicit val bogusConverter: TypeConverter[Bogus] = (s: String) => Bogus(s)

      val params: ParamsType = FakeParams(Map("a" -> "buffybuffy"))

      params.getAs[Bogus]("a") must beSome

      params.getAs[Bogus]("a").get aka "The bogus value" must_== Bogus("buffybuffy")

    }
  }
}


