import org.specs2.mutable.Specification

class BalanceSpec extends Specification {

  "Balance" should {
    "Return true when balanced case1" in {
      Balance.balance3("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList) === true
    }

    "Return true when balanced case 2" in {
      Balance.balance3("(if (zero? x) max (/ 1 x))".toList) === true
    }

    "Return false when not balanced case 1" in {
      Balance.balance3(":-)".toList) === false
    }

    "Return false when not balanced case 2" in {
      Balance.balance3("())(".toList) === false
    }

  }
}
