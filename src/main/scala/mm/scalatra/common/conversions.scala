package mm.scalatra.common
package conversions

import java.util.Date
import java.text.SimpleDateFormat

trait TypeConverter[T] {

  def convert(string: String): Option[T]

}

object Trap {
  def apply[B](f: => B): Either[Exception, B] = try {
    Right(f)
  } catch {
    case e: Exception => Left(e)
  }
}

trait DefaultConversion {

  case class Safe[T](f: (String) => T) extends TypeConverter[T] {
    def convert(value: String) = Trap {
      f(value)
    } match {
      case Right(t: T) => Option(t)
      case _ => None
    }
  }

  case class SafeOption[T](f: (String) => Option[T]) extends TypeConverter[T] {
    def convert(value: String) = Trap {
      f(value)
    } match {
      case Right(t: Option[T]) => t
      case _ => None
    }
  }


  implicit def safeConversion[T](f: (String) => T): TypeConverter[T] = Safe(f)

  implicit def safeConversionOption[T](f: (String) => Option[T]): TypeConverter[T] = SafeOption(f)

}

trait ImplicitConversions extends DefaultConversion {

  implicit val stringToBoolean: TypeConverter[Boolean] = Safe(_.toBoolean)

  implicit val stringToFloat: TypeConverter[Float] = Safe(_.toFloat)

  implicit val stringToDouble: TypeConverter[Double] = Safe(_.toDouble)

  implicit val stringToByte: TypeConverter[Byte] = Safe(_.toByte)

  implicit val stringToShort: TypeConverter[Short] = Safe(_.toShort)

  implicit val stringToInt: TypeConverter[Int] = Safe(_.toInt)

  implicit val stringToLong: TypeConverter[Long] = Safe(_.toLong)

  implicit val stringToSelf: TypeConverter[String] = Safe(s => s)

  def stringToDate(format: => String): TypeConverter[Date] = Safe(new SimpleDateFormat(format).parse(_))

  def stringToSeq[T](elementConverter: TypeConverter[T], separator: String = ","): TypeConverter[Seq[T]] = Safe(s => s.split(separator).flatMap(elementConverter.convert(_)))
}

object Conversions extends DefaultConversion with ImplicitConversions {

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