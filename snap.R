# Take a normal pair/permutation generating function (assumed to ignore jokers when matching cards up),
# and add pair contributions from the jokers. This will consequently reduce the number of normal pairs
# to otherwise find, and increase the number of cards consumed.
with.joker.pairs <- function(fn) {
	
  all.joker.blocks <- function(jokers) {
    if (sum(jokers) > 2)
      c(list(jokers), all.joker.blocks(next.blocks(jokers)))
    else if (sum(jokers) == 2)
      c(list(2), list(0))
    else
      list(0)
  }
  
  joker.blocks.allowed <- function(ranks, suits, pairs, cards) {
    function(j) {
      joker.cards <- sum(j)
      joker.pairs <- if (joker.cards > 0) sum(j-1) else 0
      joker.pairs <= pairs && joker.cards <= cards && (pairs-joker.pairs) <= floor((suits-1)*min(ranks*suits, cards-joker.cards)/suits)
    }
  }

  function(ranks, suits, jokers, pairs, cards, blocks) {
    if (jokers < 2)
      fn(ranks, suits, jokers, pairs, cards, blocks) # cannot make joker pairs
    else {
      joker.blocks <- Filter(joker.blocks.allowed(ranks, suits, pairs, cards), all.joker.blocks(jokers))
      results <- lapply(joker.blocks, function(j) {
        jokers.used <- sum(j)
        joker.pairs <- if (jokers.used > 0) sum(j-1) else 0
        joker.perms <- block.permutations(1, jokers, jokers.used > 0, j)
        results <- lapply(fn(ranks, suits, jokers-jokers.used, pairs-joker.pairs, cards-jokers.used, blocks), function(x) {
          c(x[1] * joker.perms, x[2] + jokers.used)
        })
      })
      unlist(results, recursive=FALSE)
    }
  }
}


block.permutations <- function(ranks, suits, ranks.used, blocks) {
  if (ranks.used == 0)
    1
  else {
    f <- function(acc, n) {
      existing.perms <- acc[1]
      suit.cards.remaining <- acc[2]
      new.perms <- prod((suit.cards.remaining-n+1):suit.cards.remaining)
      c(existing.perms * new.perms, suit.cards.remaining-n)
    }
    degeneracy <- prod(sapply(table(blocks), factorial))
    choose(ranks, ranks.used) * (Reduce(f, blocks, c(1, suits))[1] / degeneracy)^ranks.used
  }
}


next.blocks <- function(blocks) {
  total <- sum(blocks)
  bigger.than.two = Filter(function(n) n>2, blocks)
  # If total cards < 4, we can't split into separate groups of pairs, already atomic.
  # If no cards are > 2, or only one is and it's 3, we cannot split further.
  # If we cannot split further, jump down to the next group size.
  if (total < 4 || length(bigger.than.two) == 0 || (length(bigger.than.two) == 1 && bigger.than.two[1] == 3))
    total - 1
  else {
    # Strategy: try and split into 2 halves first, then reduce size of first group until it terminates at 2.
    # Apply this policy recursively to generate successive blocks.
    spread <- function(blocks) {
      total <- sum(blocks)
      count <- length(blocks)
      if (count == 1)
        c(floor(total/2), total - floor(total/2))
      else if (blocks[1] == 2)
        c(2, spread(blocks[2:count]))
      else {
        blocks[1] <- blocks[1] - 1
        blocks[2] <- blocks[2] + 1
        blocks
      }
    }
    spread(blocks)
  }
}


permutations <- function(ranks, suits, jokers, pairs, cards, blocks) {
  block.cards <- sum(blocks)
  if (block.cards == 1)
    list(c(1,0))
  else {
    block.pairs <- sum(blocks-1)
    min.blocks <- max(0, pairs-ranks*(block.cards-2))
    max.blocks <- ranks
    n = Filter(function(n) (pairs >= n*block.pairs && n*block.cards <= cards), min.blocks:max.blocks)
    results <- lapply(n, function(n) {
      pairs.created <- n*block.pairs
      cards.used <- n*block.cards
      others <- permutations(ranks-n, suits, jokers, pairs-pairs.created, cards-cards.used, next.blocks(blocks))
      lapply(others, function(x) c(x[1] * block.permutations(ranks, suits, n, blocks), x[2] + cards.used))
    })
    unlist(results, recursive=FALSE)
  }
}


#
#  Calculate the probability of a pair occurring in the first k cards
#  k - the depth
#
pair.probability <- function(k, ranks, suits, jokers=0) {
  deck <- ranks*suits + jokers
  at.least.p.pairs <- function(p) {
    probability <- function(tuple) {
      pair.permutations <- tuple[1]
      cards.used <- tuple[2]
      pair.permutations * prod((k-cards.used+1):(k-p)) / prod((deck-cards.used+1):deck)
    }
    fn <- with.joker.pairs(permutations)
    perms <- fn(ranks, suits, jokers, p, k, blocks=min(suits, k))
    sum(sapply(perms, probability))
  }

  # Use the inclusion-exclusion principle to calculate the probability of a pair existing
  max.pairs <- floor((suits-1)*k/suits)
  if (max.pairs > 0) sum(sapply(1:max.pairs, function(p) ((-1)^(p+1) * at.least.p.pairs(p)))) else 0
}


#
#  Calculate the probability of the first pair occurring at k
#  k - the location of the first pair
#
first.pair.probability <- function(k, ranks, suits) {
  if (k < 2)
    0
  else {
    # m is the number of cards making up the given pair at k (ie 2 if first pair in pack)
    # When called recursively, this function returns the probability that a pair precedes all the
    # pairs in a block of m cards of the same suit, where the block terminates at k.
    probability.of.pair.before <- function(k, ranks, suits, m=2) {
      if (k <= m)
        0
      else if (m == suits)
        pair.probability(k-suits, ranks-1, suits)
      else
        pair.probability(k-m, ranks-1, suits, suits-m) +
          (1-probability.of.pair.before(k, ranks, suits, m+1))*(suits-m)/(ranks*suits-m)
    }
    (1-probability.of.pair.before(k, ranks, suits))*(suits-1)/(ranks*suits-1)
  }
}


#
#  Calculate the mean location of the first pair
#
first.pair.mean.location <- function(ranks, suits) {
  k <- 1:(ranks*suits)
  return (sum(k * sapply(k, function(k) first.pair.probability(k, ranks, suits))))
}


#
#  Calculate the mean location of the first pair, using all available cores
#
first.pair.mean.location.parallel <- function(ranks, suits) {
  k <- 1:(ranks*suits)
  cores <- parallel::detectCores()
  fun <- function(k) {
    print(sprintf("%s %s", date(), k))
    first.pair.probability(k, ranks, suits)
  }
  results <- unlist(parallel::mclapply(k, fun, mc.cores=cores))
  sum(k * results)
}
