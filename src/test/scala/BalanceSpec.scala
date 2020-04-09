import org.specs2.mutable.Specification

class BalanceSpec extends Specification {

  "BalanceReduce" should {
    "Return true when balanced case 1" in {
      Balance.balanceWithReduce("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList) === true
    }

    "Return true when balanced case 2" in {
      Balance.balanceWithReduce("(if (zero? x) max (/ 1 x))".toList) === true
    }

    "Return false when not balanced case 1" in {
      Balance.balanceWithReduce(":-)".toList) === false
    }

    "Return false when not balanced case 2" in {
      Balance.balanceWithReduce("())(".toList) === false
    }

  }

  "BalanceRaphi" should {
    "Return true when balanced case1" in {
      Balance.balanceRaphi("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList) === true
    }

    "Return true when balanced case 2" in {
      Balance.balanceRaphi("(if (zero? x) max (/ 1 x))".toList) === true
    }

    "Return false when not balanced case 1" in {
      Balance.balanceRaphi(":-)".toList) === false
    }

    "Return false when not balanced case 2" in {
      Balance.balanceRaphi("())(".toList) === false
    }
  }
}
