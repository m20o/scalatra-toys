package mm.scalatra
package command


trait ValidatedBinding[T] extends Binding[T] {

  def validation: Either[Rejected[T], Accepted[T]]

  def valid = validation.isRight

  def rejected: Option[Rejected[T]] = validation match {
    case Left(r: Rejected[T]) => Some(r)
    case _ => None
  }
}

sealed trait FieldValidationResult[T]

final case class Rejected[T](error: Option[String] = None, rejected: Option[T] = None) extends FieldValidationResult[T]

final case class Accepted[T]() extends FieldValidationResult[T]

// RejectField message
case class RejectField[T](rejected: Option[T], message: String = null) {
  lazy val messageOption = Option(message)
}


sealed class FunctionValidatedBinding[T](binding: Binding[T], val validator: PartialFunction[Option[T], RejectField[T]]) extends ValidatedBinding[T] {

  val field = binding.source
  override val source = binding.source
  override lazy val value = binding.value

  override def toString() = source + " with " + validator

  val default: PartialFunction[Option[T], RejectField[T]] = {
    case _ => null
  }

  lazy val validation = {
    val result = Option((validator.orElse(default)).apply(value))
    result match {
      case Some(e: RejectField[_]) => Left(Rejected(e.messageOption, e.rejected))
      case None => Right(Accepted[T]())
    }
  }
}


sealed class BindingValidationSupport[T](command: Command, binding: Binding[T]) {

  def validate(validator: PartialFunction[Option[T], RejectField[T]]): ValidatedBinding[T] = {
    val newBinding = new FunctionValidatedBinding[T](binding, validator)
    command.bindings += (binding.field.name -> newBinding)
    newBinding
  }

}


trait ValidationSupport {

  this: Command =>

  private var _valid: Option[Boolean] = None

  private var _fieldErrors: Map[String, Rejected[_]] = Map.empty

  def valid = _valid

  def fieldErrors = _fieldErrors

  implicit def binding2Validated[T](binding: Binding[T]) = new BindingValidationSupport[T](this, binding)

  protected lazy val validatableBindings = bindings.collect {
    case (_, b: ValidatedBinding[_]) => b
  }

  override def postBinding {
    _valid = Some(validatableBindings.view.forall(_.valid))
    _fieldErrors = validatableBindings.filterNot(_.valid).map((b: ValidatedBinding[_]) => (b.field.name, b.rejected.get)).toMap
  }

}