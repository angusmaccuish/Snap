with.jokers <- function(fn) {
  function(ranks, suits, jokers, pairs, cards, blocks=c(suits)) {
    block.size <- sum(blocks)
    joker.blocks <- {
      can.accomodate.jokers <- (pairs >= block.size-1 && jokers >= block.size && cards >= block.size && block.size > 1)
      max.pairs.possible.with.non.jokers <- ranks*(block.size-1)
      min.joker.blocks <- (can.accomodate.jokers && max.pairs.possible.with.non.jokers < pairs)
      max.joker.blocks <- if (can.accomodate.jokers) floor(min(jokers, cards)/block.size) else 0
      Filter(function(j) { pairs >= j*(block.size-1) }, min.joker.blocks:max.joker.blocks)
    } 
    results <- lapply(joker.blocks, function(j) {
      jokers.used <- j*block.size
      joker.pairs <- j*(block.size-1)
      lapply(fn(ranks, suits, jokers-jokers.used, pairs-joker.pairs, cards-jokers.used, blocks), function(x) {
        c(x[1] * prod((jokers-block.size+1):jokers)^j, x[2] + jokers.used)
      })
    })
    unlist(results, recursive=FALSE)
  }
}


block.permutations <- function(ranks, suits, ranks.used, blocks) {
  f <- function(acc, n) {
    existing.perms <- acc[1]
    suit.cards.remaining <- acc[2]
    new.perms <- prod((suit.cards.remaining-n+1):suit.cards.remaining)
    c(existing.perms * new.perms, suit.cards.remaining-n)
  }
  degeneracy <- prod(sapply(table(blocks), factorial))
  choose(ranks, ranks.used) * (Reduce(f, blocks, c(1, suits))[1] / degeneracy)^ranks.used 
}


next.blocks <- function(blocks) {
  block.cards <- sum(blocks)
  if (length(blocks) > 1 || block.cards < 4)
    list(c(block.cards-1)) # either single block of 3 cards or less, or a collection of blocks
  else
    if (block.cards == 4) list(c(2,2)) 
    else if (block.cards == 5) list(c(2,3))
    else if (block.cards == 6) list(c(2,4), c(3,3), c(2,2,2))
    else stop("Don't support more than 6 suits right now!")
}


f <- with.jokers(function(ranks, suits, jokers, pairs, cards, blocks=c(suits)) {
  block.cards <- sum(blocks)
  if (block.cards == 1) 
    list(c(1,0))
  else {
    block.pairs <- sum(sapply(blocks, function(n) (n-1)))
    min.blocks <- max(0, pairs-ranks*(block.cards-2))
    max.blocks <- ranks
    n = Filter(function(n) (pairs >= n*block.pairs && n*block.cards <= cards), min.blocks:max.blocks)
    results <- lapply(n, function(n) {
	  pairs.created <- n*block.pairs
	  cards.used <- n*block.cards
	  others <- unlist(
		lapply(next.blocks(blocks), function(blocks) f(ranks-n, suits, jokers, pairs-pairs.created, cards-cards.used, blocks)),
        recursive=FALSE
      )
      lapply(others, function(x) c(x[1] * block.permutations(ranks, suits, n, blocks), x[2] + cards.used))
    })
    unlist(results, recursive=FALSE)
  }
})


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
    perms <- f(ranks, suits, jokers, p, k)
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
  slice <- function(x,n) {
    N <- length(x)
    lapply(seq(1,N,n), function(i) x[i:min(i+n-1,N)])
  }
  cards <- ranks*suits
  cores <- parallel::detectCores()
  batches <- slice(1:cards, floor((cards+1)/cores))
  fun <- function(k) {
    print(k)
    sum(k * sapply(k, function(k) first.pair.probability(k, ranks, suits)))
  }
  results <- unlist(parallel::mclapply(batches, fun, mc.cores=cores))
  sum(results)
}
