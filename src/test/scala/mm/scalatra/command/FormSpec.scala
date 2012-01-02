package mm.scalatra
package command

import org.specs2.mutable.Specification


trait KnownFields {

  import Fields._

  val upperCaseName = asGeneric[String]("name", _.toUpperCase)

  val lowerCaseSurname = asGeneric[String]("surname", _.toLowerCase())

  val age = asInt("age")

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

    "process 'params' Map and bind matching values to specific " in {
      val form = new WithBinding
      val params = Map("name" -> "John", "surname" -> "Doe")
      form.process(params)
      form.a.value must_== Some(params("name").toUpperCase)
      form.lower.value must_== Some(params("surname").toLowerCase)
    }

  }
}

class WithValidation extends WithBinding with ValidationSupport {


  val legalAge = bind(age).validate {
    case s@Some(yo: Int) if yo < 18 => RejectField(s, "Your age must be at least of 18")
    case None => RejectField[Int](None, "Age field is required")
  }

}


class ValidatableSpec extends Specification {

  "The 'ValidationSupport' trait" should {

    "do normal binding within 'process'" in {

      val ageValidatedForm = new WithValidation
      val params = Map("name" -> "John", "surname" -> "Doe", "age" -> "15")

      ageValidatedForm.process(params)

      ageValidatedForm.a.value must_== Some(params("name").toUpperCase)
      ageValidatedForm.lower.value must_== Some(params("surname").toLowerCase)
      ageValidatedForm.age.value must_== Some(15)

    }

    "validate only 'validatable bindings' within process" in {

      val ageValidatedForm = new WithValidation
      val params = Map("name" -> "John", "surname" -> "Doe", "age" -> "15")

      ageValidatedForm.valid must beNone

      ageValidatedForm.process(params)

      ageValidatedForm.valid must beSome[Boolean]


      ageValidatedForm.valid.get must beFalse
      ageValidatedForm.fieldErrors aka "validation error list" must haveKey("age")
      ageValidatedForm.fieldErrors.get("age").get.asInstanceOf[Rejected[Int]] aka "the validation error" must_== (Rejected(Some("Your age must be at least of 18"), Some(15)))
    }

  }
}
