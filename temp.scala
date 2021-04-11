def remove(elem: Int, list: List[Int]): List[Int] = {
  if (list.isEmpty) {
    List()
  } else if (list.head == elem) {
    list.tail
  } else {
    list.head::(remove(elem, list.tail))
  }
}

def countChange(money: Int, coins: List[Int]): Int = {
  coins.fold(0) ((acc, curr) => {
    (money, curr) match {
      case (money, _) if money < 0 => 0
      case (0, _) => 1
      case (money, curr) => {
        val next_coins = remove(curr, coins)
        acc + countChange(money - curr, next_coins)
      }
    }
  })
}
