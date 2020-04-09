import scala.annotation.tailrec

object Balance {

  def balanceRaphi(chars: List[Char]): Boolean = {

    @tailrec
    def loop(acc: Int, check: Boolean, chars: List[Char]): Boolean =
      if (chars.isEmpty) {
        Console.println(s"Finished result: $check ")
        acc == -1
      } else {
        val currentChar   = chars.head
        val isHeadClosing = isClosing(currentChar)
        val newAcc        = accAdj(isHeadClosing, acc)
        val isParentRes   = isParent(newAcc)
        val remaining     = chars.tail
        Console.println(
          s"Current char is: $currentChar isClosing: $isHeadClosing newAcc: $newAcc isParent: $isParentRes remaining: $remaining"
        )
        loop(
          newAcc,
          isParentRes,
          remaining
        )
      }

    def isClosing(zeichen: Char): Boolean =
      zeichen == ')'

    def accCalc(zeichen: Char, alt: Int): Int =
      if (zeichen == '(') alt + 1
      else alt;

    def accAdj(adj: Boolean, alt: Int): Int =
      if (adj) accCalc(chars.head, alt - 1)
      else accCalc(chars.head, alt)

    def isParent(score: Int): Boolean =
      score > -1

    loop(0, true, chars)
  }

  def balance(chars: List[Char]): Boolean = {
    def balanceInternal(c: List[Char], count: Int): Boolean = c match {
      case Nil                   => count == 0
      case ')' :: _ if count < 1 => false
      case ')' :: xs             => balanceInternal(xs, count - 1)
      case '(' :: xs             => balanceInternal(xs, count + 1)
      case _ :: xs               => balanceInternal(xs, count)
    }
    balanceInternal(chars, 0)
  }

  def balanceWithReduce(chars: List[Char]): Boolean =
    chars.foldLeft(0)((count: Int, char: Char) =>
      char match {
        case ')' if count < 1 => count + 1
        case ')'              => count - 1
        case '('              => count + 1
        case _                => count
      }
    ) == 0

}
