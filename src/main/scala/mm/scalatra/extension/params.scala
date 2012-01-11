package mm.scalatra.extension

import mm.scalatra.common.conversions.{TypeConverter, DefaultImplicitConversions}


trait ScalatraParamsImplicits {

  self: DefaultImplicitConversions =>

  type ParamsType = MultiMapHeadView[String, String] with MapWithIndifferentAccess[String]

  sealed case class TypedParams(params: ParamsType) {
    def getAs[T <: Any](name: String)(implicit tc: TypeConverter[T]): Option[T] = params.get(name).flatMap(tc.convert(_))
  }

  implicit def toTypedParams(params: ParamsType) = new TypedParams(params)

}

object Params extends ScalatraParamsImplicits with DefaultImplicitConversions {


}