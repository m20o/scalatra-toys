package mm.scalatra.command
package field

import scala.math._
import org.specs2.mutable.Specification
import java.util.Date
import java.text.{SimpleDateFormat, DateFormat}
import mm.scalatra.common.conversions.TypeConverter

class FieldSpec extends Specification {

  "BasicField class " should {


    case class TestingFieldImpl[T](override val name: String, parse: (String) => T) extends BasicField[T](name)(new TypeConverter[T] {
      def convert(string: String) = Option(parse(string))
    })

    "provide an 'update' method that updates 'originalValue" in {

      val updater = new TestingFieldImpl[String]("name", (s: String) => null)
      updater.originalValue must beNull

      val newValue = System.currentTimeMillis().toString
      updater.update(newValue)

      updater.originalValue must_== newValue
    }

    "delegate to TypeConverter conversions from String to specific type T" in {

      val stringToDate = new TestingFieldImpl[Date]("date", (s: String) => new Date(s.toLong))
      val now = new Date

      stringToDate.update(now.getTime.toString)

      stringToDate.value must beSome(now)
    }

  }

  object Fields extends ImplicitCommonFields

  "ImplicitCommonFields object" should {

    import Fields._

    "provide Field[Boolean]" in {

      val field = testImplicitFieldType[Boolean]
      setAndCheckValue(field, true)
    }

    "provide Field[Float]" in {
      val field = testImplicitFieldType[Float]
      val num = random.toFloat
      setAndCheckValue(field, num)
    }

    "provide Field[Double]" in {
      val field = testImplicitFieldType[Double]
      val num = random.toDouble
      setAndCheckValue(field, num)
    }

    "provide Field[Int]" in {
      val field = testImplicitFieldType[Int]
      val num = random.toInt
      setAndCheckValue(field, num)
    }

    "provide Field[Byte]" in {
      val field = testImplicitFieldType[Byte]
      val num = random.toByte
      setAndCheckValue(field, num)
    }

    "provide Field[Short]" in {
      val field = testImplicitFieldType[Short]
      val num = random.toShort
      setAndCheckValue(field, num)
    }

    "provide Field[Long]" in {
      val field = testImplicitFieldType[Long]
      val num = random.toLong
      setAndCheckValue(field, num)
    }

    "provide Field[String] that should treat blank strings an None" in {
      val field = newField[String](Fields.asString(_))
      field.update("   ")
      field.value must beNone
    }

    "provide Field[String] that should treat blank strings as Some() if required" in {
      val field = newField[String](Fields.asString(_, false))
      field.update("   ")
      field.value must beSome[String]
      field.value.get must_== "   "
    }

    "provide Field[String] with an equivalent Tuple argument syntax" in {
      val field = newField[String]((s: String) => Fields.asString(s -> false))
      field.update("   ")
      field.value must beSome[String]
      field.value.get must_== "   "
    }

    "provide Field[Date] with a default DateFormat" in {
      val field = newField[Date](Fields.asDate(_))
      val now = newDateWithFormattedString(dateFormatFor())
      field.update(now._2)
      field.value must beSome[Date]
      field.value.get must_== now._1
    }

    "provide Field[Date] with a given date format" in {
      val format = "yyyyMMddHHmmsss"
      val field = newField[Date](Fields.asDate(_, format))
      val now = newDateWithFormattedString(dateFormatFor(format))
      field.update(now._2)
      field.value must beSome[Date]
      field.value.get must_== now._1
    }

    "provide Field[Date] with an equivalent Tuple-based argument syntax" in {
      val format = "yyyyMMddHHmmsss"
      val field = newField[Date]((s: String) => Fields.asDateWithStringFormat(s -> format))
      val now = newDateWithFormattedString(dateFormatFor(format))
      field.update(now._2)
      field.value must beSome[Date]
      field.value.get must_== now._1
    }

    "provide a generic implementation of Field[Seq[T]] which delegates inner conversion" in {
      import Fields._
      val field = newField[Seq[Int]](asSeq[Int](_, (s: String) => s.toInt))
      field.update("1,2,3,4,5")
      field.value must beSome[Seq[Int]]
      field.value.get must_== List(1, 2, 3, 4, 5).toSeq
    }
  }

  def dateFormatFor(format: String = null): DateFormat = if (format == null) DateFormat.getInstance() else new SimpleDateFormat(format)

  def newDateWithFormattedString(format: DateFormat) = {
    val date = new Date
    (format.parse(format.format(date)) -> format.format(date))
  }


  def testImplicitFieldType[T: TypeConverter] = {


    import Fields._

    val fieldname = randomFieldName

    val field: Field[T] = fieldname

    field.name must_== fieldname
    field must beAnInstanceOf[Field[T]]



    field
  }

  def newField[T](f: (String) => Field[T]): Field[T] = f(randomFieldName)

  def setAndCheckValue[T](field: Field[T], value: T) = {
    field.originalValue must beNull
    field.update(value.toString)
    field.value.get must_== value
  }

  def randomFieldName = "field_" + random
}