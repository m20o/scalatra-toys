package mm.scalatra.form

trait Binding[T] {

  def field: Field[T]

  lazy val value = field.value

  val source = field
}

trait Form {

  self: Form =>

  type BindingType[T] = Binding[T] with ValidationSupport[T]
  type Params = Map[String, String]

  private case class BindingWithValidation[T](field: Field[T], form: Form = self) extends Binding[T] with ValidationSupport[T]

  protected[form] var bindings: Map[String, Binding[_]] = Map()

  def bind[T](field: Field[T]): BindingType[T] = {
    val newBinding = new BindingWithValidation[T](field)
    bindings += (field.name -> newBinding)
    newBinding
  }

  def process(params: Params) = {
    for (
      entry <- params;
      binding <- bindings.get(entry._1);
      field = binding.source;
      value = entry._2
    ) {
      field.update(value)
    }
    this
  }
}

sealed trait ValidatedBinding[T] extends Binding[T] {

  def validate: Box[T]

}

sealed case class Box[T <: Any](fieldName: String, error: Option[String] = None, rejected: Option[T] = None) {
  val valid: Boolean = error == None
}


// FieldError message
case class FieldError[T](rejected: Option[T], message: String = null) {

  lazy val messageOption = Option(message)

}


sealed trait FunctionalValidatedBinding[T] extends ValidatedBinding[T] {

  def validator: PartialFunction[Option[T], FieldError[T]]

  val default: PartialFunction[Option[T], FieldError[T]] = {
    case _ => null
  }

  def validate = {
    val result = Option((validator.orElse(default)).apply(value))
    result match {
      case Some(e: FieldError[T]) => Box(this.source.name, e.messageOption, e.rejected)
      case None => Box[T](this.source.name)
    }
  }
}

trait ValidationSupport[T] {

  self: Form#BindingType[T] =>

  def form: Form

  private class Delegated[T](binding: Form#BindingType[T], val validator: PartialFunction[Option[T], FieldError[T]]) extends FunctionalValidatedBinding[T] {
    val field = binding.source
    override val source = binding.source
    override lazy val value = binding.value

    override def toString() = source + " with " + validator

    self.form.bindings += (field.name -> this)
  }

  def validate(validator: PartialFunction[Option[T], FieldError[T]]): ValidatedBinding[T] = {
    new Delegated[T](self, validator)
  }
}


case class Validation(private val results: Iterable[Box[_]]) {

  val valid = results.forall(_.valid)

  val errors = results.filterNot(_.valid).map(x => (x.fieldName, (x.error, x.rejected))).toMap

}

trait Validatable extends Form {

  private var validationResult: Validation = null

  def validationOption: Option[Validation] = Option(validationResult)

  def validation: Validation = validationResult

  protected lazy val validatableBindings = bindings.collect {
    case (_, b: ValidatedBinding[_]) => b
  }

  protected def doValidation(): Validation = {
    val results = for (validator <- validatableBindings) yield validator.validate
    new Validation(results.view)
  }

  abstract override def process(params: Validatable#Params) = {
    super.process(params)
    this.validationResult = doValidation()
    this
  }
}