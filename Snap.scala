package snap

import scala.util.Random

object Snap extends App with Utils {

  /**
   * Example: r=13, s=4 (standard pack of playing cards)
   * We have a block of 4 matching cards x 2: ranksUsed = 2 blocks = List(4)
   * We have two single pairs for a single rank: ranksUsed = 1, blocks = List(2,2)
   * We have a single pair x 4: ranksUsed = 4, blocks = List(2)
   *
   * This method returns the number of permuations (where the order of the blocks is unimportant)
   */
  def blockPermutations(ranks: Int, suits: Int, ranksUsed: Int, blocks: List[Int]): BigInt = {
    if (blocks.sum > suits) throw new IllegalArgumentException("The total number of cards per rank cannot exceed the number of suits")
    if (ranksUsed == 0)
      1
    else {
      val (_, perms) = blocks.foldLeft((suits, 1)) {
        (cards, block) =>
          cards match {
            case (cardsRemaining, existingPerms) =>
              val newPerms = ((cardsRemaining - block + 1) to cardsRemaining).product
              (cardsRemaining - block, existingPerms * newPerms)
          }
      }
      val degeneracy = (blocks.groupBy(block => block) map { case (_, values) => factorial(values.length) }).product
      binomialCoefficient(ranks, ranksUsed) * (perms / degeneracy).pow(ranksUsed)
    }
  }

  /**
   * Return the next set of blocks to consider for a suit.
   * Best demonstrated with examples:
   * List(4) -> List(2,2)
   * List(2,2) -> List(3)
   * List(3) -> List(2)
   *
   * Expected usage: start with List(suits). Then this method returns the next configuration of cards
   * to consider. We move from maximum number of cards available -> 2 for a single pair
   */
  def nextBlocks(blocks: List[Int]): List[Int] = {
    val total = blocks.sum
    if (total < 2) throw new IllegalArgumentException("Cards cannot be broken down further")
    val biggerThanTwo = blocks filter (_ > 2)
    if (total < 4 || biggerThanTwo.isEmpty || biggerThanTwo == Seq(3))
      List(total - 1)
    else {
      def spread(blocks: List[Int]): List[Int] = {
        (blocks: @unchecked) match {
          case block :: Nil =>
            val total = blocks.sum
            val half = math.floor(total / 2).toInt
            List(half, total - half)
          case 2 :: tail =>
            2 :: spread(tail)
          case first :: second :: tail =>
            (first - 1) :: (second + 1) :: tail
        }
      }
      spread(blocks)
    }
  }

  /**
   * Permutations
   */
  def permutations(ranks: Int, suits: Int, jokers: Int, pairs: Int, cards: Int, blocks: List[Int]): List[(BigInt, Int)] = {
    if (blocks == List(1))
      List((1, 0))
    else {
      val blockCards = blocks.sum
      val blockPairs = (blocks map (_ - 1)).sum
      val minBlocks = math.max(0, pairs - ranks * (blockCards - 2)) // deduct max number of pairs available for next block down..
      val maxBlocks = ranks
      (minBlocks to maxBlocks).toList filter (n => pairs >= n * blockPairs && n * blockCards <= cards) flatMap {
        n =>
          // Use n * blocks
          // Then recursively find remaining pairs (if any) using other block configurations, remembering that
          // the number of remaining ranks is reduced by n, with the pairs required/cards used also suitably updated
          val pairsCreated = n * blockPairs
          val cardsUsed = n * blockCards
          val others = permutations(ranks - n, suits, jokers, pairs - pairsCreated, cards - cardsUsed, nextBlocks(blocks))
          val blockPerms = blockPermutations(ranks, suits, n, blocks)
          others map { case (x, y) => (x * blockPerms, y + cardsUsed) }
      }
    }
  }

  /**
   * Incorporate jokers
   */
  def withJokers(permutations: (Int, Int, Int, Int, Int, List[Int]) => List[(BigInt, Int)]): (Int, Int, Int, Int, Int, List[Int]) => List[(BigInt, Int)] = {

    def allJokerPairs(jokers: List[Int]): List[List[Int]] = {
      val jokerCards = jokers.sum
      if (jokerCards > 2) jokers :: allJokerPairs(nextBlocks(jokers))
      else if (jokerCards == 2) List(List(2), List(0))
      else List(List(0))
    }

    def areJokerBlocksAllowed(ranks: Int, suits: Int, pairs: Int, cards: Int): List[Int] => Boolean = jokers => {
      val jokerCards = jokers.sum
      val jokerPairs = if (jokerCards > 0) (jokers map (_ - 1) sum) else 0
      jokerPairs <= pairs && jokerCards <= cards && (pairs - jokerPairs) <= (BigDecimal((suits - 1) * math.min(ranks * suits, cards - jokerCards)) / BigDecimal(suits)).intValue
    }

    (ranks, suits, jokers, pairs, cards, blocks) =>

      if (jokers < 2)
        permutations(ranks, suits, jokers, pairs, cards, blocks)
      else {
        allJokerPairs(List(jokers)) filter areJokerBlocksAllowed(ranks, suits, pairs, cards) flatMap { j =>
          val jokersUsed = j.sum
          val jokerPairs = if (jokersUsed > 0) (j map (_ - 1) sum) else 0
          val jokerPerms = blockPermutations(1, jokers, if (jokersUsed > 0) 1 else 0, j)
          permutations(ranks, suits, jokers - jokersUsed, pairs - jokerPairs, cards - jokersUsed, blocks) map {
            case (x, y) => (x * jokerPerms, y + jokersUsed)
          }
        }
      }
  }

  /**
   * Probability of a pair being found in the first k cards
   */
  def pairProbability(k: Int, ranks: Int, suits: Int, jokers: Int = 0): BigDecimal = {
    val deck = ranks * suits + jokers
    def atLeastPPairs(p: Int): BigDecimal = {
      def probability(perm: (BigInt, Int)) = perm match {
        case (perms, cards) => BigDecimal(perms * (BigInt(k - cards + 1) to BigInt(k - p)).product) / BigDecimal((BigInt(deck - cards + 1) to BigInt(deck)).product)
      }
      val permutationsWithJokers = withJokers(permutations)
      permutationsWithJokers(ranks, suits, jokers, p, k, List(math.min(suits, k))) map probability sum
    }
    val maxPairs = math.floor((suits - 1) * k / suits).toInt
    if (maxPairs > 0) ((1 to maxPairs) map (p => BigDecimal(-1).pow(p + 1) * atLeastPPairs(p)) sum) else 0.0
  }

  /**
   * First pair probability
   */
  def firstPairProbability(k: Int, ranks: Int, suits: Int): BigDecimal = {
    if (k < 2)
      0.0
    else {
      def probabilityOfPairBefore(k: Int, ranks: Int, suits: Int, m: Int = 2): BigDecimal = {
        if (k <= m) 0.0
        else if (m == suits) pairProbability(k - suits, ranks - 1, suits)
        else pairProbability(k - m, ranks - 1, suits, suits - m) + (1.0 - probabilityOfPairBefore(k, ranks, suits, m + 1)) * BigDecimal(suits - m) / BigDecimal(ranks * suits - m)
      }
      (1.0 - probabilityOfPairBefore(k, ranks, suits)) * BigDecimal(suits - 1) / BigDecimal(ranks * suits - 1)
    }
  }

  /**
   * Mean location of first pair
   */
  def firstPairMeanLocation(ranks: Int, suits: Int): BigDecimal = { Random.shuffle(1 to (ranks * suits)).par map (k => { println(k); k * firstPairProbability(k, ranks, suits) }) sum }

  val Array(ranks, suits) = args map (_.toInt)
  val start = System.currentTimeMillis
  println(s"Mean location of first pair for ranks=$ranks, suits = $suits is: %s".format(firstPairMeanLocation(ranks, suits)))
  println("Time taken: %s (secs)".format((System.currentTimeMillis - start) / 1000.0))
}

trait Utils {
  def factorial(n: BigInt): BigInt = (BigInt(1) to n).product
  def binomialCoefficient(n: Int, k: Int) = (BigInt(n - k + 1) to n).product / (BigInt(1) to k).product
}
