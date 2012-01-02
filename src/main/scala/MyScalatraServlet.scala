import java.text.DateFormat
import java.util._
import mm.scalatra.form._
import org.scalatra._
import java.net.URL
import scalate.ScalateSupport

sealed class ValidadedBean extends MyBean with Validatable


class MyScalatraServlet extends ScalatraServlet
with ScalateSupport
with FormSupport
{

  
  get("/test") {

    val form = formOf[ValidadedBean]

    println("form " + form + " -> class " + form.getClass.getName)

    val name = form.name.value
    val age = form.age.value
    val date = form.birthday.value

    val ages: Option[Int] = form.age

    val validation = form.validation

    <html>
      <body>
        <h1>Hello, world!</h1>
        Say
        <a href="hello-scalate">hello to Scalate</a>
          <br/>
        <p>Locale is
        </p>
        <p>Name is
          {name.getOrElse("BoH")}
        </p>
        <p>Age is
          {age.getOrElse(-1)}
        </p>
        <p>Birthday is
          {date.getOrElse(null)}
        </p>
        <p>Form is valid?
          {validation.valid}
        </p>
        <p>Errors
          {validation.errors}
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

case class B(var age: String)

class MyBean extends Form {

  import Fields._

   def a = {
     val ciccia = B("asdf")
   }
  /*
  val name = bind(asString("name").validate({
    case None => FieldError[String](null, "Field can't be null")
  }))
  */

  val name = bind[String]("name").validate {
    case None => FieldError[String](null, "Il campo 'nome' è richiesto")
  }

  /*
  val age = bind(asInt("age").validate({
    case None => FieldError(None, "Bisogna specificare l'età dell'utente")
    case Some(i: Int) if i < 18 => FieldError(Some(i), "L'utente deve essere maggiorenne")
  }))
  */

  val age = bind[Int]("age") validate {
    case s@Some(i: Int) if i <= 18 => FieldError(s, "Devi essere maggiorenne!")
    case None => FieldError[Int](null, "Il campo non può essere vuoto")
  }




  val birthday = bind[Date]("birthday" -> "yyyyMMdd") validate {
    case (d: Date) => null
    case _ => FieldError(None, "Il campo 'birthday' è richiesto")
  }

  val codes = bind[Seq[Int]]("codes" -> ((s: String) => s.toInt))

  val caps = bind[Seq[String]]("caps") validate {
    case s@Some(arr: Seq[String]) if arr.length == 0 => FieldError(s, "At least one caps is required")
    case None => FieldError(None, "Il campo caps è richiesto")
  }
}
