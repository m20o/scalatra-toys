package mm.scalatra.common
package conversions

import java.util.Date
import java.text.SimpleDateFormat

trait ImplicitConversions {

  type TypeConverter[T] = ((String) => Option[T])

  object SafeConverter {

    def apply[T](f: (String) => T): TypeConverter[T] = (value: String) => try {
      Option(f(value))
    } catch {
      case _ => None
    }

  }

  implicit val stringToBoolean: TypeConverter[Boolean] = SafeConverter(_.toBoolean)

  implicit val stringToFloat: TypeConverter[Float] = SafeConverter(_.toFloat)

  implicit val stringToDouble: TypeConverter[Double] = SafeConverter(_.toDouble)

  implicit val stringToByte: TypeConverter[Byte] = SafeConverter(_.toByte)

  implicit val stringToShort: TypeConverter[Short] = SafeConverter(_.toShort)

  implicit val stringToInt: TypeConverter[Int] = SafeConverter(_.toInt)

  implicit val stringToLong: TypeConverter[Long] = SafeConverter(_.toLong)

  implicit val stringToSelf: TypeConverter[String] = SafeConverter(s => s)

  implicit def stringToDate(format: => String): TypeConverter[Date] = SafeConverter(new SimpleDateFormat(format).parse(_))

  implicit def stringToSeq[T](elementConverter: TypeConverter[T], separator: String = ","): TypeConverter[Seq[T]] = SafeConverter(s => s.split(separator).flatMap(elementConverter(_)))
}

object `package` extends ImplicitConversions
