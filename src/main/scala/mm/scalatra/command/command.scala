package mm.scalatra.command

trait Binding[T] {

  def field: Field[T]

  lazy val value = field.value

  val source = field
}


trait Command {

  self: Command =>

  type Params = Map[String, String]

  private case class <>[T](field: Field[T], command: Command = self) extends Binding[T]

  private[command] var bindings: Map[String, Binding[_]] = Map()

  def bind[T](field: Field[T]): Binding[T] = {
    val newBinding = new <>[T](field)
    bindings += (field.name -> newBinding)
    newBinding
  }

  protected def beforeBinding {}

  protected def postBinding {}

  final def process(params: Params) = {
    beforeBinding
    for (
      entry <- params;
      binding <- bindings.get(entry._1);
      field = binding.source;
      value = entry._2
    ) {
      field.update(value)
    }
    postBinding
    this
  }
}

