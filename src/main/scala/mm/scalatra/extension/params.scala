package mm.scalatra.extension

import mm.scalatra.common.conversions.{TypeConverter, DefaultImplicitConversions}
import java.util.Date
import org.scalatra.util.{MultiMap, MapWithIndifferentAccess, MultiMapHeadView}

/**
 * Add some implicits
 */
trait ScalatraParamsImplicits {

  self: DefaultImplicitConversions =>

  type ParamsType = MultiMapHeadView[String, String] with MapWithIndifferentAccess[String]

  type MultiParamsType = MultiMap


  sealed class TypedParams(params: ParamsType) {

    def getAs[T <: Any](name: String)(implicit tc: TypeConverter[T]): Option[T] = params.get(name).flatMap(tc.convert(_))

    def getAs[T <: Date](nameAndFormat: (String, String)): Option[Date] = getAs(nameAndFormat._1)(stringToDate(nameAndFormat._2))

  }

  sealed class TypedMultiParams(multiParams: MultiParamsType) {

    def getAs[T <: Any](name: String)(implicit tc: TypeConverter[T]): Option[Seq[T]] = multiParams.get(name) map {
      s =>
        s.flatMap(tc.convert(_))
    }

    def getAs[T <: Date](nameAndFormat: (String, String)): Option[Seq[Date]] = getAs(nameAndFormat._1)(stringToDate(nameAndFormat._2))
  }

  implicit def toTypedParams(params: ParamsType) = new TypedParams(params)

  implicit def toTypedMultiParams(params: MultiParamsType) = new TypedMultiParams(params)
}

object Params extends ScalatraParamsImplicits with DefaultImplicitConversions