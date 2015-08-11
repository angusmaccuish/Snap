########################################################################
#                                                                      #
#  Calculate the probability of a pair occurring in the first k cards  #
#  k - the depth                                                       #
#                                                                      #
########################################################################
pair.probability <- function(k, ranks, suits, jokers=0, fn=pairs) {
  if (jokers >= suits) stop("Jokers must be less than suits")
  deck <- ranks*suits + jokers
  at.least.p.pairs <- function(p) {
    probability <- function(tuple) {
      pair.permutations <- tuple[1]
      cards.used <- tuple[2]
      pair.permutations * prod((k-cards.used+1):(k-p)) / prod((deck-cards.used+1):deck)
    }
    perms <- fn(suits)(ranks, jokers, p, k)
    sum(sapply(perms, probability))
  }

  # Use the inclusion-exclusion principle to calculate the probability of a pair existing
  max.pairs <- floor((suits-1)*k/suits)
  if (max.pairs > 0) sum(sapply(1:max.pairs, function(p) { return ((-1)^(p+1) * at.least.p.pairs(p)) })) else 0
}


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


first.pair.mean.location <- function(ranks, suits) {
  k <- 1:(ranks*suits)
  return (sum(k * sapply(k, function(k) { first.pair.probability(k, ranks, suits) })))	
}


pairs <- function(suits) {
  with.jokers <- function(block.size, fn) {
    function(ranks, jokers, pairs, cards) {
      joker.blocks <- {
        can.accomodate.jokers <- (pairs >= block.size-1 && jokers >= block.size && cards >= block.size)
        max.pairs.possible.with.non.jokers <- ranks*(block.size-1)
        min.joker.blocks <- (can.accomodate.jokers && max.pairs.possible.with.non.jokers < pairs) # TODO
        max.joker.blocks <- if (can.accomodate.jokers) floor(min(jokers, cards)/block.size) else 0
        Filter(function(j) { pairs >= j*(block.size-1) }, min.joker.blocks:max.joker.blocks)
      }
      results <- lapply(joker.blocks, function(j) {
        jokers.used <- j*block.size
        joker.pairs <- j*(block.size-1)
        lapply(fn(ranks, jokers-jokers.used, pairs-joker.pairs, cards-jokers.used), function(x) {
          c(x[1] * choose(jokers, block.size)^j * factorial(block.size)^j, x[2] + jokers.used)
        })
      })
      unlist(results, recursive=FALSE)
    }
  }

  two.cards <- with.jokers(2, function(ranks, jokers, pairs, cards) {
    perms <- choose(ranks, pairs) * choose(suits, 2)^pairs * factorial(2)^pairs
    cards <- 2*pairs
    list(c(perms, cards))
  })
	
  three.cards <- with.jokers(3, function(ranks, jokers, pairs, cards) {
    t.min <- max(0, pairs-ranks) # this is only true for whole pack, need to consider k !
    t.max <- ranks
    t <- Filter(function(t) { pairs >= 2*t }, t.min:t.max)
    f <- function(t) {
      lapply(two.cards(ranks-t, jokers, pairs-2*t, cards-3*t), function(x) { 
        perms <- x[1] * choose(ranks, t) * choose(suits, 3)^t * factorial(3)^t
        cards <- x[2] + 3*t
        c(perms, cards)
      })
    }
    unlist(lapply(t, f), recursive=FALSE)
  })
	
  four.cards <- with.jokers(4, function(ranks, jokers, pairs, cards) {
    q.min <- max(0, pairs-2*ranks)
    q.max <- ranks
    q <- Filter(function(q) { pairs >= 3*q }, q.min:q.max)
    f <- function(q) {
      d.min <- 0
      d.max <- ranks-q
      d <- Filter(function(d) { pairs >= (3*q + 2*d) }, d.min:d.max)
      f <- function(d) {
        lapply(three.cards(ranks-q-d, jokers, pairs-3*q-2*d, cards-4*q-4*d), function(x) {
	      perms <- x[1] * choose(ranks, q) * choose(suits, 4)^q * factorial(4)^q * choose(ranks-q, d) * 3^d * factorial(2)^(2*d)
	      cards <- x[2] + 4*q + 4*d
          c(perms, cards)
        })	
      }
      unlist(lapply(d, f), recursive=FALSE)
    }
    unlist(lapply(q, f), recursive=FALSE)
  })
	
  switch(suits, NULL, two.cards, three.cards, four.cards)
}


