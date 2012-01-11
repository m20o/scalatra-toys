package mm.scalatra.common.conversions

import org.specs2.mutable.Specification
import java.util.{Date, Calendar}

class ConversionsSpecs extends Specification {


  "The TypeConverted[T] trait (type class)" should {

    "have a convert method that convert String into Option[T]" in {

      val underTest = new TypeConverter[String] {
        def convert(string: String) = Option(string.toUpperCase)
      }

      underTest.convert("hello") must_== Some("HELLO")
    }
  }

  "The TypeConverterSupport trait" should {

    object WithImplicit extends TypeConverterSupport

    type SafeType[T] = TypeConverterSupport#Safe[T]

    "provide Safe TypeConverter type " in {
      classOf[TypeConverter[_]] must beAssignableFrom[SafeType[_]]
    }

    "Safe TypeConverter should return None in case of exceptions" in {

      import WithImplicit.Safe

      val converter = new Safe[String]((s) => throw new Exception(s))

      converter.convert("anything") must beNone
    }

    "implicitly convert a (String)=>T function into a TypeConveter[T]" in {

      import WithImplicit._

      case class A(i: Int)

      val stringToA = (s: String) => A(s.toInt)

      val converted: TypeConverter[A] = stringToA

      converted must_== Safe(stringToA)
    }

    "implicitly convert a (String)=>Option[T] function into a TypeConveter[T]" in {

      import WithImplicit._

      case class A(i: Int)

      val stringToOptionA = (s: String) => Option(s).map((v: String) => A(v.toInt))

      val converted: TypeConverter[A] = stringToOptionA

      converted must_== SafeOption(stringToOptionA)
    }

  }

  "The DefaultImplicitConversions trait" should {

    object Impl extends DefaultImplicitConversions {

      def testFor[T](source: String, expected: Option[T])(implicit t: TypeConverter[T]) = {
        t.convert(source) must_== expected
      }
    }

    "provide implicit VALs for basic types" in {
      import Impl._
      testFor("Hello", Some("Hello"))
      testFor("1.34d", Some(1.34d))
      testFor("1.23f", Some(1.23f))
      testFor("true", Some(true))
      testFor[Long]("12345678", Some(12345678L))
      testFor[Short]("1", Some(1.toShort))
      testFor("1234567890", Some(1234567890))
    }

    "provide DEF conversion for Date" in {

      val dateConverter = Impl.stringToDate("S") // milliseconds

      val cal = Calendar.getInstance
      val currentMs = cal.get(Calendar.MILLISECOND)
      val converted: Option[Date] = dateConverter.convert(currentMs.toString)

      converted aka "The converted Date value" must beSome[Date]

      cal.setTime(converted.get)

      cal.get(Calendar.MILLISECOND) aka "The extracted milliseconds from converted Date" must_== currentMs
    }

    "provide DEF conversion for Seq" in {

      import Impl._

      def testConversion[T](args: (String, Seq[T]))(implicit t: TypeConverter[T]) = {
        val (source, expected) = args
        Impl.stringToSeq(t).convert(source).get must containAllOf(expected).inOrder
      }

      testConversion("1,2,3" -> List(1, 2, 3))
      testConversion("a,b,c,,e" -> List("a", "b", "c", "", "e"))

      case class B(v: Int)
      implicit val bConv: TypeConverter[B] = (s: String) => B(s.toInt * 2)

      testConversion("1,2,3" -> List(B(2), B(4), B(6)))
    }
  }

  "The Conversions object" should {

    import Conversions._

    "Pimp String type with as[T]" in {

      // Some value tests
      "2".as[Int] must beSome(2)
      "2.0".as[Int] must beNone
      "2.0".as[Double] must beSome(2.0)

      "false".as[Boolean] must beSome(false)
    }

    "Pimp String type with as[T] that delegates to implicit TypeConverter[T]" in {

      // A type and its type converter
      case class B(v: Int)
      implicit val bConv: TypeConverter[B] = (s: String) => B(s.toInt * 2)

      "10".as[B] should beSome(B(20))
    }

    "Pimp String type with asDate implicit that require a format" in {
      "20120101".asDate("yyyyMMdd") must beSome[Date]
    }

    "Pimp String type with asSeq[T]" in {
      val b = "1,2,3,4,5".asSeq[Int]
      b must beSome[Seq[Int]]
      b.get must containAllOf(List(1, 2, 3, 4, 5))
    }

    "Pimp String type with asSeq[T] with separator" in {
      val b = "1 2 3 4 5".asSeq[Int](" ")
      b must beSome[Seq[Int]]
      b.get must containAllOf(List(1, 2, 3, 4, 5))
    }

    "Pimp String type with asSeq[T] with an implicit TypeConverter" in {
      case class C(s: String)
      implicit val cconv: TypeConverter[C] = (s: String) => C(s)

      val b = "1,2,3".asSeq[C]
      b must beSome[Seq[C]]
      b.get must containAllOf(List(C("1"), C("2"), C("3")))
    }

  }
}