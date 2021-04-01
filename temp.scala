  def innerBalance(chars: List[Char], openedCount: Int): Boolean = {
    print(chars)
    print("\n")
    print(openedCount)
    print("\n")
    chars match {
      case ('('::cs) => innerBalance(cs, openedCount + 1)
      case (')'::cs) => {
        if (openedCount - 1 >= 0) {
          innerBalance(cs, openedCount - 1)
        } else {
          false
        }
      }
      case (_::cs) => innerBalance(cs, openedCount)
      case List() => {
        openedCount match {
          case 0 => true
          case _ => false
        }
      }
    }
  }
  def balance(chars: List[Char]): Boolean = innerBalance(chars, 0)
