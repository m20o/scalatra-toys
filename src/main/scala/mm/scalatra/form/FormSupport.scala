package mm.scalatra
package form

import org.scalatra.{RouteMatcher, ScalatraKernel}
import org.scalatra.util.MultiMap


/**
 * Support class
 */
trait FormSupport  {

  this : ScalatraKernel =>

  implicit def bindingValue[T](b : Binding[T]) : Option[T] = b.value

  private class FormRouteMatcher[T <: Validatable : Manifest] extends RouteMatcher {

    override def apply() = if (formOf[T].validation.valid) Some(MultiMap()) else None

    override def toString = "[valid form guard]"
  }
  
  def formOf[T <: Form : Manifest] : T = {
    val formClass = manifest[T].erasure
    val formClassRequestKey = "_form_" + formClass.getName
    val existingForm = request.get(formClassRequestKey).map(_.asInstanceOf[T])
    existingForm match {
      case None => {
        println("Creating new Form")
        val form = formClass.asInstanceOf[Class[T]].newInstance()
        form.process(params)
        request.setAttribute(formClassRequestKey, form)
        form
      }
      case Some(form: T) => {
        println("Reusing cached form")
        form
      }
    }
  }


  def ifValid[T <: Validatable : Manifest] : RouteMatcher = new FormRouteMatcher[T]
}