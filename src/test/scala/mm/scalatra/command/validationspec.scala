package mm.scalatra.command
package validation

import org.specs2.mutable.Specification

class WithValidation extends WithBinding with ValidationSupport {

  val notRequiredCap = bind(cap) validate  {
    case s @ Some(capval: Int) if capval <= 100 => RejectField(s, "CAP must be > 100")
  }

  val legalAge = bind(age).validate {
    case s@Some(yo: Int) if yo < 18 => RejectField(s, "Your age must be at least of 18")
    case None => RejectField[Int](None, "Age field is required")
  }

}


class ValidationSupportSpec extends Specification {

  "The 'ValidationSupport' trait" should {

    "do normal binding within 'doBinding'" in {

      val ageValidatedForm = new WithValidation
      val params = Map("name" -> "John", "surname" -> "Doe", "age" -> "15")

      ageValidatedForm.doBinding(params)

      ageValidatedForm.a.value must_== Some(params("name").toUpperCase)
      ageValidatedForm.lower.value must_== Some(params("surname").toLowerCase)
      ageValidatedForm.age.value must_== Some(15)

    }

    "validate only 'validatable bindings' within doBinding" in {

      val ageValidatedForm = new WithValidation
      val params = Map("name" -> "John", "surname" -> "Doe", "age" -> "15")

      ageValidatedForm.valid must beNone

      ageValidatedForm.doBinding(params)

      ageValidatedForm.valid must beSome[Boolean]

      ageValidatedForm.valid.get must beFalse
      ageValidatedForm.fieldErrors aka "validation error list" must haveKey("age")
      ageValidatedForm.fieldErrors.get("age").get.asInstanceOf[Rejected[Int]] aka "the validation error" must_== (Rejected(Some("Your age must be at least of 18"), Some(15)))
    }


    "evaluate non-exaustive validation as 'accepted'" in {
      val formUnderTest = new WithValidation
      val params = Map("name" -> "John", "surname" -> "Doe", "age" -> "20")

      params must not haveKey("cap")
      
      formUnderTest.doBinding(params)
      formUnderTest.valid must beSome[Boolean]
      formUnderTest.valid.get must beTrue

      formUnderTest.notRequiredCap.value must beNone
      formUnderTest.notRequiredCap.valid must beTrue
    }

  }
}


