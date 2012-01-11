package mm.scalatra
package command

import org.scalatra.{RouteMatcher, ScalatraKernel}
import org.scalatra.util.MultiMap

import validation._

/**
 * Support for [[mm.scalatra.command.Command]] binding and validation.
 */
trait FormSupport {

  this: ScalatraKernel =>

  type CommandValidatedType = Command with ValidationSupport

  /**
   * Implicitly convert a [[mm.scalatra.command.Binding]] value to an [[scala.Option]]
   */
  implicit def bindingValue[T](b: Binding[T]): Option[T] = b.value

  /**
   * Create and bind a [[mm.scalatra.command.Command]] of the given type with the current Scalatra params.
   *
   * For every command type, creation and binding is performed only once and then stored into
   * a request attribute.
   */
  def formOf[T <: Command : Manifest]: T = {
    val formClass = manifest[T].erasure
    val formClassRequestKey = "_form_" + formClass.getName
    val existingForm = request.get(formClassRequestKey).map(_.asInstanceOf[T])
    existingForm match {
      case None => {
        val newForm = formClass.asInstanceOf[Class[T]].newInstance()
        newForm.doBinding(params)
        request.setAttribute(formClassRequestKey, newForm)
        newForm
      }
      case Some(form) => form
    }
  }


  private class FormRouteMatcher[T <: CommandValidatedType : Manifest] extends RouteMatcher {

    override def apply() = if (formOf[T].valid.get) Some(MultiMap()) else None

    override def toString = "[valid command guard]"
  }

  /**
   * Create a [[org.scalatra.RouteMatcher]] that evaluates '''true''' only if a command is valid. See
   * [[mm.scalatra.command.validation.ValidationSupport]] for details.
   */
  def ifValid[T <: CommandValidatedType : Manifest]: RouteMatcher = new FormRouteMatcher[T]
}