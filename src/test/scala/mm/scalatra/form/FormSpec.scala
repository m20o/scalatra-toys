package mm.scalatra
package form

import org.specs2.mutable.Specification

import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith

import Fields._

trait KnownFields {

  import Fields._

  val upperCaseName = asGeneric[String]("name", _.toUpperCase)

  val lowerCaseSurname = asGeneric[String]("surname", _.toLowerCase())

  val age = asInt("age")

}

class WithBinding extends Form with KnownFields {

  import Fields._

  val a = bind(upperCaseName)

  val lower = bind(lowerCaseSurname)
}


class FormSpec extends Specification {

  "The 'Form' trait" should {

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

class WithValidation extends WithBinding with Validatable {


  val legalAge = bind(age).validate {
    case s@Some(yo: Int) if yo < 18 => FieldError(s, "Your age must be at least of 18")
    case None => FieldError[Int](None, "Age field is required")
  }

}


class ValidatableSpec extends Specification {

  "The 'Validatable' trait" should {

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

      ageValidatedForm.validationOption must beNone

      ageValidatedForm.process(params)

      ageValidatedForm.validationOption must beSome[Validation]

      val validationResult = ageValidatedForm.validation

      validationResult.valid must beFalse
      validationResult.errors aka "validation error map" must haveKey("age")
      validationResult.errors.get("age").get aka "the validation error message" must_== (Some("Your age must be at least of 18"), Some(15))
    }

  }
}
