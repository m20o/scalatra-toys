import java.util._
import mm.scalatra.command._
import mm.scalatra.command.validation._
import org.scalatra._
import scalate.ScalateSupport


class MyScalatraServlet extends ScalatraServlet with ScalateSupport with FormSupport {


  get("/test") {

    val form = formOf[MyBean]

    val name = form.name
    val surname = form.surname
    val age = form.age
    val data = form.data

    def showError(b: ValidatedBinding[_]) = b.rejected.map(_.error.getOrElse("This field is wrong")).getOrElse("")

    def printValue(b: ValidatedBinding[_]) = b.originalValue

    <html>
      <body>
        <h1>Submit this nice form please!</h1>
        <form method="GET">
          <p>Name
            <input type="text" name="name" value={printValue(name)}></input>{showError(name)}
          </p>
          <p>Surname
            <input type="text" name="surname" value={printValue(surname)}></input>{showError(surname)}
          </p>
          <p>Age
            <input type="number" name="age" value={printValue(age)}></input>{showError(age)}
          </p>
          <p>Date
            <input type="text" name="data" value={printValue(data)}></input>{showError(data)}
          </p>
            <br/>
            <input type="submit"/>
        </form>
        <p>Name is
          {name.getOrElse("BoH")}
        </p>
        <p>Age is
          {age.getOrElse(-1)}
        </p>
        <p>Form is valid?
          {form.valid}
        </p>
        <p>Errors
          {form.fieldErrors}
        </p>
      </body>
    </html>
  }

  get("/test", ifValid[MyBean]) {

    val form = formOf[MyBean]

    <html>
      <body>
        <h1>Submitted VALID data:</h1>
        <p>Name:
          {form.name.value.getOrElse("N/A")}{form.surname.value.getOrElse("N/A")}
        </p>
        <p>Age:
          {form.age.value.get}
        </p>
        <p>Date:
          {form.data.value.getOrElse("N/A")}
        </p>
      </body>
    </html>

  }


  notFound {
    // Try to render a ScalateTemplate if no route matched
    findTemplate(requestPath) map {
      path =>
        contentType = "text/html"
        layoutTemplate(path)
    } orElse serveStaticResource() getOrElse resourceNotFound()
  }
}


object MyBean {

  def invalidBlank[T]: PartialFunction[Option[T], RejectField[T]] = {
    case None => RejectField[T](None, "Field  is required")
  }

  def legalAge: PartialFunction[Option[Int], RejectField[Int]] = {
    case s@Some(age: Int) if age < 18 => RejectField(s, "Legal age is required")
  }

}

class MyBean extends Command with ValidationSupport {

  import MyBean._
  import Command._

  val name = bind[String]("name").validate(invalidBlank)

  val surname = bind[String]("surname").validate(invalidBlank)

  val data = bind[Date]("data" -> "yyyyMMdd") validate {
    case Some(d: Date) if d.before(new Date(0)) => RejectField[Date](None, "La data non è valida")
  }

  val age = bind[Int]("age") validate (legalAge orElse invalidBlank[Int])

}
