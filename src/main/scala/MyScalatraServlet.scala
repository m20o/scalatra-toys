import mm.scalatra.extension.Params
import org.scalatra.ScalatraServlet


/**
 * Created by IntelliJ IDEA.
 * User: Max
 * Date: 11/01/12
 * Time: 17:40
 * To change this template use File | Settings | File Templates.
 */

class MyScalatraServlet extends ScalatraServlet {

  get("/") {

    import Params._

    val a = multiParams.getAs[Int]("name")

    val b = params.getAsDate("date" -> "yyyyMMdd")

    val c : Option[Seq[Float]] = params.getAsSeq("a")

    <html>
      <body>
        Hello world
      </body>
    </html>
  }

}