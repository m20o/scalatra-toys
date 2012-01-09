package mm.scalatra.extension

import org.scalatra.util.{MapWithIndifferentAccess, MultiMapHeadView}
import mm.scalatra.common.conversions.ImplicitConversions


trait ScalatraParamsImplicits {

  self: ImplicitConversions =>

  type ParamsType = MultiMapHeadView[String, String] with MapWithIndifferentAccess[String]

  sealed case class TypedParams(params: ParamsType) {

    def getAs[T <: Any : TypeConverter](name: String): Option[T] = {
      val converter = implicitly[TypeConverter[T]]
      params.get(name).flatMap(converter(_))
    }
  }

  implicit def toTypedParams(params: ParamsType) = new TypedParams(params)

}

object Params extends ScalatraParamsImplicits with ImplicitConversions {


}
