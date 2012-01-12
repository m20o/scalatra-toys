package mm.scalatra.command
package field

import java.util.Date
import java.text._
import mm.scalatra.common.conversions._


/**
 * Command object field. Supports basic attributes.
 *
 * '''TODO''' work in progress. Field is actually used to tie web pameters with typed value but it's somewhat redundant.
 * Will be probably changed in the future.
 */
trait Field[T] {

  /**
   * The field name.
   */
  def name: String

  /**
   * The original value of this field.
   */
  def originalValue: String

  /**
   * Converted value.
   */
  def value: Option[T]

  /**
   * Update the original value of this field.
   *
   */
  protected[command] def originalValue_=(value: String): Unit

}

/**
 * Field base class. Use an implicit [[mm.scalatra.common.conversions.TypeConverter]]
 * as conversion strategy between strings and required types.
 */
class BasicField[T](val name: String)(implicit val tc: TypeConverter[T]) extends Field[T] {

  private var _original: String = null

  def originalValue: String = _original

  lazy val value: Option[T] = tc.convert(_original)

  def originalValue_=(value: String) {
    _original = value
  }
}

/**
 * Commonly-used field implementations factory.
 */
trait ImplicitCommonFields extends DefaultImplicitConversions {

  private def blankStringConverter(blankAsNull: Boolean): TypeConverter[String] = (s: String) => Option(s) match {
    case x@Some(value: String) if (!blankAsNull || value.trim().size > 0) => x
    case _ => None
  }

  def asGeneric[T](name: String, f: (String) => T): Field[T] = asGeneric(name)(f)

  def asGeneric[T](name: String)(implicit tc: TypeConverter[T]): Field[T] = new BasicField[T](name)

  implicit def asType[T <: Any : TypeConverter](name: String): Field[T] = new BasicField[T](name)

  implicit def asString(name: String, blankAsNull: Boolean = true): Field[String] = new BasicField(name)(blankStringConverter(blankAsNull))

  implicit def asString(param: (String, Boolean)): Field[String] = asString(param._1, param._2)

  def asDate(name: String, format: DateFormat = DateFormat.getInstance()): Field[Date] = new BasicField(name)(stringToDateFormat(format))

  def asDate(name: String, format: String): Field[Date] = new BasicField(name)(stringToDate(format))

  implicit def asDateWithStringFormat(param: (String, String)): Field[Date] = asDate(param._1, param._2)

  implicit def asDateWithDateFormat(param: (String, DateFormat)): Field[Date] = asDate(param._1, param._2)

  def asSeq[T](name: String, converter: TypeConverter[T]): Field[Seq[T]] = new BasicField(name)(stringToSeq(converter))

  implicit def asSeqWithConverter[T](param: (String, (String) => T)): Field[Seq[T]] = new BasicField(param._1)(stringToSeq(param._2))

  implicit def asStringSeq(name: String): Field[Seq[String]] = new BasicField(name)(stringToSeq(stringToSelf))

  implicit def asStringSeq(param: (String, String)): Field[Seq[String]] = new BasicField(param._1)(stringToSeq(stringToSelf, param._2))
}
