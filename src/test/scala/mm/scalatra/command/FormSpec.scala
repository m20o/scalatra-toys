package mm.scalatra
package command

import org.specs2.mutable.Specification
import java.lang.System


trait KnownFields {

  import Command._

  val upperCaseName = asGeneric[String]("name", _.toUpperCase)

  val lowerCaseSurname = asGeneric[String]("surname", _.toLowerCase())

  val age = asInt("age")

  val cap = asInt("cap")

}

class WithBinding extends Command with KnownFields {

  val a = bind(upperCaseName)

  val lower = bind(lowerCaseSurname)
}



class FormSpec extends Specification {

  "The 'Command' trait" should {

    "bind and register a 'Field[T]' instance returning a Binding[T]" in {
      val form = new WithBinding
      form.a must beAnInstanceOf[Binding[String]]
      form.a.field must_== form.upperCaseName
    }

    "have unprocessed binding values set to 'None'" in {
      val form = new WithBinding
      form.a.value must_== None
      form.lower.value must_== None
    }

    "doBinding 'params' Map and bind matching values to specific " in {
      val form = new WithBinding
      val params = Map("name" -> "John", "surname" -> "Doe")
      form.doBinding(params)
      form.a.value must_== Some(params("name").toUpperCase)
      form.lower.value must_== Some(params("surname").toLowerCase)
    }
    
    "provide pluggable actions processed 'BEFORE' binding " in {
      import System._

      trait PreBindAction extends WithBinding {

        var timestamp : Long  = _
        
        beforeBinding {
          a.field.originalValue must beNull[String]
          timestamp = currentTimeMillis()
        }
      }

      val form = new WithBinding with PreBindAction
      val params = Map("name" -> "John", "surname" -> "Doe")

      form.timestamp must_== 0L

      form.doBinding(params)
      
      form.timestamp must be_<(currentTimeMillis())
      form.a.field.originalValue must_== params("name")
    }

    "provide pluggable actions processed 'AFTER' binding " in {
      import System._

      trait AfterBindAction extends WithBinding {

        private var _fullname: String = _

        def fullName: Option[String] = Option { _fullname }
        
        afterBinding {
          _fullname = a.value.get + " " + lower.value.get
        }
      }

      val params = Map("name" -> "John", "surname" -> "Doe")
      val form = new WithBinding with AfterBindAction
      
      form.fullName must beNone

      form.doBinding(params)
      
      form.fullName must beSome[String]
      form.fullName.get must_== params("name").toUpperCase + " " + params("surname").toLowerCase

    }
  }
}


