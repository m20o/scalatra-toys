package mm.scalatra.common
package conversions

import java.util.Date
import java.text.{DateFormat, SimpleDateFormat}

/**
 * Type converter type class.
 */
trait TypeConverter[T] {

  /**
   * Convert a string to the specific Option type.
   */
  def convert(string: String): Option[T]
}

/**
 * Helper for wrapping exceptions into [[scala.Either]] type instance.
 */
object Trap {
  def apply[B](f: => B): Either[Exception, B] = try {
    Right(f)
  } catch {
    case e: Exception => Left(e)
  }
}

/**
 * Support types and implicits for [[mm.scalatra.common.conversions.TypeConverter]].
 */
trait TypeConverterSupport {

  /**
   * Wrapper a function `(String) => T into a exception-safe [[mm.scalatra.common.conversions.TypeConverter]]
   */
  case class Safe[T](f: (String) => T) extends TypeConverter[T] {
    def convert(value: String) = Trap {
      f(value)
    } match {
      case Right(t) => Option(t)
      case _ => None
    }
  }

  /**
   * Wrapper a function `(String) => Option[T]` into a exception-safe [[mm.scalatra.common.conversions.TypeConverter]]
   */
  case class SafeOption[T](f: (String) => Option[T]) extends TypeConverter[T] {
    def convert(value: String) = Trap {
      f(value)
    } match {
      case Right(t) => t
      case _ => None
    }
  }

  /**
   * Implicit convert a `(String) => T` function into a `TypeConverter[T]
   */
  implicit def safeConversion[T](f: (String) => T): TypeConverter[T] = Safe(f)

  /**
   * Implicit convert a `(String) => Option[T]` function into a `TypeConverter[T]
   */
  implicit def safeConversionOption[T](f: (String) => Option[T]): TypeConverter[T] = SafeOption(f)
}

/**
 * Default implicit TypeConverter definitions.
 */
trait DefaultImplicitConversions extends TypeConverterSupport {

  implicit val stringToBoolean: TypeConverter[Boolean] = Safe(_.toBoolean)

  implicit val stringToFloat: TypeConverter[Float] = Safe(_.toFloat)

  implicit val stringToDouble: TypeConverter[Double] = Safe(_.toDouble)

  implicit val stringToByte: TypeConverter[Byte] = Safe(_.toByte)

  implicit val stringToShort: TypeConverter[Short] = Safe(_.toShort)

  implicit val stringToInt: TypeConverter[Int] = Safe(_.toInt)

  implicit val stringToLong: TypeConverter[Long] = Safe(_.toLong)

  implicit val stringToSelf: TypeConverter[String] = Safe(s => s)

  /**
   * Return a [[mm.scalatra.common.conversions.TypeConverter]] for [[java.util.Date]] string with a given format.
   *
   * @param format - the date format, as specified in [[java.text.SimpleDateFormat]].
   * @return the type converter.
   */
  def stringToDate(format: => String): TypeConverter[Date] = stringToDateFormat(new SimpleDateFormat(format))

  def stringToDateFormat(format: => DateFormat): TypeConverter[Date] = Safe(format.parse(_))

  def stringToSeq[T](elementConverter: TypeConverter[T], separator: String = ","): TypeConverter[Seq[T]] = Safe(s => s.split(separator).flatMap(elementConverter.convert(_)))
}

object Conversions extends TypeConverterSupport with DefaultImplicitConversions {

  class ValConversion(source: String) {
    def as[T: TypeConverter]: Option[T] = implicitly[TypeConverter[T]].convert(source)
  }

  class DateConversion(source: String) {
    def asDate(format: String): Option[Date] = stringToDate(format).convert(source)
  }

  class SeqConversion(source: String) {
    def asSeq[T: TypeConverter]: Option[Seq[T]] = stringToSeq(implicitly[TypeConverter[T]]).convert(source)

    def asSeq[T: TypeConverter](separator: String): Option[Seq[T]] = stringToSeq(implicitly[TypeConverter[T]], separator).convert(source)
  }

  implicit def stringToValTypeConversion(source: String) = new ValConversion(source)

  implicit def stringToDateConversion(source: String) = new DateConversion(source)

  implicit def stringToSeqConversion(source: String) = new SeqConversion(source)
}