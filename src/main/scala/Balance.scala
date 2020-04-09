import scala.annotation.tailrec

object Balance {

  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def loop(acc: Int, check: Boolean, chars: List[Char]): Boolean = {
      println(s" acc: $acc check: $check  chars: $chars")
      if ((acc == -1) || !chars.isEmpty) check
      else {
        val currentChar   = chars.head
        val isHeadClosing = isClosing(chars.head)
        val newAcc        = accAdj(isHeadClosing, acc)
        println(s" current char is: $currentChar $newAcc:")
        loop(
          newAcc,
          isParent(newAcc),
          chars.tail
        )
      }
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

  def balance2(chars: List[Char]): Boolean = {
    def inner(c: List[Char], count: Int): Boolean = c match {
      case Nil                   => count == 0
      case ')' :: _ if count < 1 => false
      case ')' :: xs             => inner(xs, count - 1)
      case '(' :: xs             => inner(xs, count + 1)
      case _ :: xs               => inner(xs, count)
    }

    inner(chars, 0)
  }

  def balance3(chars: List[Char]): Boolean =
    chars.foldLeft(0)((count: Int, char: Char) =>
      char match {
        case ')' if count < 1 => count + 1
        case ')'              => count - 1
        case '('              => count + 1
        case _                => count
      }
    ) == 0

}
