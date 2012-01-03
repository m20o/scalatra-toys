package mm.scalatra.command
package validation


/**
 * Based trait for field value validation
 */
sealed trait FieldValidationResult[T]

/**
 * Return value for a failed field validation.
 *
 * @param error an error message that can be show to the user
 * @param rejected the rejected value
 */
final case class Rejected[T](error: Option[String] = None, rejected: Option[T] = None) extends FieldValidationResult[T]

/**
 * Return value for a successful field validation.
 */
final case class Accepted[T]() extends FieldValidationResult[T]

/**
 * Reject a field value with an optional error message.
 * @param rejected the rejected value
 * @param message the (optional) error message.
 */
case class RejectField[T](rejected: Option[T], message: String = null) {
  lazy val messageOption = Option(message)
}

/**
 * A field [[mm.scalatra.command.Binding]] which value has been validated.
 */
trait ValidatedBinding[T] extends Binding[T] {

  /**
   * Result of validation. Either one of @Rejected or @Accepted
   */
  def validation: Either[Rejected[T], Accepted[T]]

  /**
   * Check whether the the field value conforms to the user requirements.
   */
  def valid = validation.isRight

  /**
   * The rejected message, if any.
   */
  def rejected: Option[Rejected[T]] = validation match {
    case Left(r: Rejected[T]) => Some(r)
    case _ => None
  }
}

/**
 * [[mm.scalatra.command.validation.ValidatedBinding]] factory object.
 */
object ValidatedBinding {

  /**
   * The field validator type.
   */
  type FieldValidator[T] = PartialFunction[Option[T], RejectField[T]]

  private def acceptAsDefault[T]: FieldValidator[T] = {
    case _ => null
  }

  /**
   * Tie an existing binding with validator function.
   * @param binding the existing binding
   * @param validator the validation funciton
   */
  def apply[T](binding: Binding[T], validator: FieldValidator[T]): ValidatedBinding[T] = new ValidatedBinding[T] {

    override def field = binding.field

    override lazy val value = binding.value

    override def toString() = binding + " with " + validator

    lazy val validation = Option {
      (validator orElse acceptAsDefault[T]).apply(value)
    } match {
      case Some(e: RejectField[_]) => Left(Rejected(e.messageOption, e.rejected))
      case None => Right(Accepted[T]())
    }
  }
}

/**
 * Add validation capabilities to a [[mm.scalatra.command.Command]]. Validation will be performed at field level.
 *
 * For a given field binding of type ``A``, validation rules can be specified as ''partial functions'' of type
 * ``PartialFunction[Option[A], RejectField[A]]``, where [[mm.scalatra.command.validation.RejectField]] represent a
 * rejected value, through the ``validate`` method. A binding value is implicitly assumed as '''valid''' if no partial
 * function is applicable to it.
 *
 * Example:
 * {{{
 * class PersonForm extends Command with ValidationSupport {
 *
 *  import Command._
 *
 *  // f_name should be not null and at least of 3 chars.
 *  val name = bind[String]("f_name") validate {
 *    case s @ Some(text: String) if text.length < 3 => RejectField(s, "Name should be at least of 3 chars")
 *    case None => RejectField[String](None, "Name is required")
 *  }
 *
 *  // f_surname has no validation rules
 *  val surname = bind[String]("f_surname")
 *
 *  // f_age can be null but, if specified, should be greater than 0.
 *  val age = bind[Int]("f_age") validate {
 *    case s @ Some(a : Int) if a < 0 => RejectField(s, "Age should be positive")
 *  }
 * }
 * }}}
 *
 * @author mmazzarolo
 * @version 0.1
 */
trait ValidationSupport {

  this: Command =>

  private var _valid: Option[Boolean] = None

  private var _fieldErrors: Map[String, Rejected[_]] = Map.empty

  /**
   * Check whether this command is valid.
   */
  def valid: Option[Boolean] = _valid

  /**
   * Return a Map of all field validation error keyed by field binding name (NOT the name of the variable in command
   * object).
   */
  def fieldErrors: Map[String, Rejected[_]] = _fieldErrors

  /**
   * Support class for 'validate' method provided by the implicit below.
   */
  sealed class BindingValidationSupport[T](command: Command, binding: Binding[T]) {

    import ValidatedBinding._

    /**
     * Validate this binding with the given partial function.
     */
    def validate(validator: FieldValidator[T]): ValidatedBinding[T] = {
      val newBinding = apply[T](binding, validator)
      command.bindings += (binding.name -> newBinding)
      newBinding
    }
  }

  /**
   * Implicit enhancement for [[mm.scalatra.command.Binding]]
   */
  implicit def binding2Validated[T](binding: Binding[T]) = new BindingValidationSupport[T](this, binding)

  /**
   * Perform validation as afterBinding task.
   */
  afterBinding {
    val validatableBindings = bindings.collect {
      case (_, b: ValidatedBinding[_]) => b
    }
    _valid = Some(validatableBindings.view.forall(_.valid))
    _fieldErrors = validatableBindings.filterNot(_.valid).map((b: ValidatedBinding[_]) => (b.name, b.rejected.get)).toMap
  }
}