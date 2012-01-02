package mm.scalatra.command

import java.util.Date
import java.text._


/**
 * Command object field. Supports basic attributes.
 * TODO work in progress. Field is actually used to tie web pameters with typed value but it's somewhat redundant.
 */
trait Field[T] {

  /**
   * The field name.
   */
  def name: String

  /**
   * The original value of the fields,
   */
  def originalValue: String

  /**
   * Converted value.
   */
  def value: Option[T]

  /**
   * Update the current field value.
   * TODO Hack - will be removed in a near future.
   */
  def update(value: String)

}

/**
 * Field template class for implementations. Use a (String) => T function
 * as conversion strategy between strings and required types.
 */
abstract class BasicField[T](private val parse: (String) => T) extends Field[T] {

  private var _original: String = null

  def originalValue: String = _original

  lazy val value: Option[T] = try {
    Some(parse(_original))
  } catch {
    case _ => None
  }

  def update(value: String) {
    _original = value
  }
}

sealed class GenericField[T](val name: String, parse: (String) => T) extends BasicField[T](parse)

sealed class BooleanField(val name: String) extends BasicField[Boolean](_.toBoolean)

sealed class FloatField(val name: String) extends BasicField[Float](_.toFloat)

sealed class DoubleField(val name: String) extends BasicField[Double](_.toDouble)

sealed class ByteField(val name: String) extends BasicField[Byte](_.toByte)

sealed class ShortField(val name: String) extends BasicField[Short](_.toShort)

sealed class IntField(val name: String) extends BasicField[Int](_.toInt)

sealed class LongField(val name: String) extends BasicField[Long](_.toLong)

sealed class StringField(val name: String, blankAsNull: Boolean = true) extends BasicField[String](null) {
  override lazy val value = {
    Option(originalValue) match {
      case x@Some(s: String) if (!blankAsNull || s.trim().size > 0) => x
      case _ => None
    }
  }
}

sealed class DateField(val name: String, format: DateFormat = DateFormat.getDateInstance) extends BasicField[Date](format.parse(_))

sealed class SeqField[T](val name: String, elementParser: (String) => T, separator: String = ",") extends BasicField[Seq[T]](_.split(separator).map(elementParser).toSeq)

/**
 * Commonly-used field implementations factory.
 */
object Fields {

  def asGeneric[T](name: String, parser: (String) => T): Field[T] = new GenericField[T](name, parser)

  implicit def asBoolean(name: String): Field[Boolean] = new BooleanField(name)

  implicit def asFloat(name: String): Field[Float] = new FloatField(name)

  implicit def asDouble(name: String): Field[Double] = new DoubleField(name)

  implicit def asByte(name: String): Field[Byte] = new ByteField(name)

  implicit def asLong(name: String): Field[Long] = new LongField(name)

  implicit def asInt(name: String): Field[Int] = new IntField(name)

  implicit def asShort(name: String): Field[Short] = new ShortField(name)

  implicit def asString(name: String): Field[String] = new StringField(name)

  implicit def asString(name: String, blankAsNull: Boolean = true): Field[String] = new StringField(name, blankAsNull)

  implicit def asString(param: (String, Boolean)): Field[String] = asString(param._1, param._2)

  def asDate(name: String, format: DateFormat = DateFormat.getInstance()): Field[Date] = new DateField(name, format)

  def asDate(name: String, format: String): Field[Date] = asDate(name, new SimpleDateFormat(format))

  implicit def asDateWithStringFormat(param: (String, String)): Field[Date] = asDate(param._1, param._2)

  implicit def asDateWithDateFormat(param: (String, DateFormat)): Field[Date] = asDate(param._1, param._2)

  def asSeq[T](name: String, converter: (String) => T): Field[Seq[T]] = new SeqField[T](name, converter)

  implicit def asSeqWithConverter[T](param: (String, (String) => T)): Field[Seq[T]] = new SeqField[T](param._1, param._2)

  implicit def asStringSeq(name: String): Field[Seq[String]] = new SeqField[String](name, s => s)

  implicit def asStringSeq(param: (String, String)): Field[Seq[String]] = new SeqField[String](param._1, s => s, param._2)

}
