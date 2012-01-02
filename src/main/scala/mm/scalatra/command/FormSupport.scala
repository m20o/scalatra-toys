package mm.scalatra
package command

import org.scalatra.{RouteMatcher, ScalatraKernel}
import org.scalatra.util.MultiMap


/**
 * Support class
 */
trait FormSupport {

  this: ScalatraKernel =>

  type CommandValidatedType = Command with ValidationSupport

  implicit def bindingValue[T](b: Binding[T]): Option[T] = b.value

  private class FormRouteMatcher[T <: CommandValidatedType : Manifest] extends RouteMatcher {

    override def apply() = if (formOf[T].valid.get) Some(MultiMap()) else None

    override def toString = "[valid command guard]"
  }

  def formOf[T <: Command : Manifest]: T = {
    val formClass = manifest[T].erasure
    val formClassRequestKey = "_form_" + formClass.getName
    val existingForm = request.get(formClassRequestKey).map(_.asInstanceOf[T])
    existingForm match {
      case None => {
        println("Creating new Command")
        val form = formClass.asInstanceOf[Class[T]].newInstance()
        form.process(params)
        request.setAttribute(formClassRequestKey, form)
        form
      }
      case Some(form: T) => {
        println("Reusing cached command")
        form
      }
    }
  }


  def ifValid[T <: CommandValidatedType : Manifest]: RouteMatcher = new FormRouteMatcher[T]
}