###################################################################################
# Calculate theoretical probability of a pair occurring within the first k cards. #
# Hardcoded for case of suits=2 case.                                             #
###################################################################################
pair.probability.with.2.suits <- function(k, ranks=13) {
  deck <- 2*ranks
  f <- function(s) {
    cards <- 2*s
    p <- (-1)^(s+1)
    p <- p * choose(ranks, s)
    p <- p * 2^s
    p <- p / prod((deck-cards+1):deck)
    p <- p * prod((k-cards+1):(k-s))
    return (p)
  }
  max <- floor(k/2)
  p <- if (max > 0) sum(sapply(1:max, f)) else 0
  return (p)
}


################################################################
# (Unconditional) Probability that the first pair occurs at k. #
# Hardcoded for case of suits=2 case.                          #
################################################################
first.pair.probability.with.2.suits <- function(k, ranks=13) {
  probability.of.no.pair.before.pair.at.k <- 0
  if (k==2) probability.of.no.pair.before.pair.at.k <- 1
  if (k>2) {
    probability.of.no.pair.before.pair.at.k <- 1 - pair.probability.with.2.suits(k-2, ranks-1)
  }
  probability.of.pair.at.k <- 1/(2*ranks-1)
  return (probability.of.no.pair.before.pair.at.k * probability.of.pair.at.k)
}


###################################################################################
# Calculate theoretical probability of a pair occurring within the first k cards. #
# Hardcoded for case of suits=3 case.                                             #
###################################################################################
pair.probability.with.3.suits <- function(k, ranks=13) {
  deck <- 3*ranks
  outer <- function(n) {
    t.min <- max(0, n-ranks) # the minimum number of triples
    t.max <- floor(n/2) # the maximum number of triples
    inner <- function(t) {
      s <- n-2*t
      cards <- 2*s+3*t
      q <- (-1)^(n+1)
      q <- q * choose(ranks, s)
      q <- q * choose(ranks-s, t)
      q <- q * 6^(s+t)
      q <- q / prod((deck-cards+1):deck)
      q <- q * prod((k-cards+1):(k-n))
      return (q)
    }
    return (sum(sapply(t.min:t.max, inner)))
  }
  n.max <- floor(2*k/3)
  p <- if (n.max > 0) sum(sapply(1:n.max, outer)) else 0
  return (p)
}


###################################################################################
# Calculate theoretical probability of a pair occurring within the first k cards. #
# Conditioned on a joker having been dealt before the k^th card.                  #
# Hardcoded for case of suits=3 case.                                             #
###################################################################################
pair.probability.with.3.suits.given.joker.already.dealt <- function(k, ranks=13) {
  deck <- 3*ranks
  outer <- function(n) {
    t.min <- max(0, n-ranks) # the minimum number of triples
    t.max <- floor(n/2) # the maximum number of triples
    inner <- function(t) {
      s <- n-2*t
      cards <- 2*s+3*t
      q <- (-1)^(n+1)
      q <- q * choose(ranks, s)
      q <- q * choose(ranks-s, t)
      q <- q * 6^(s+t)
      q <- q / prod((deck-cards+1):deck)
      q <- q * prod((k-cards):(k-n))
      q <- q * (k-n-1)/(k-n)/(k-1)
      return (q)
    }
    return (sum(sapply(t.min:t.max, inner)))
  }
  n.max <- floor(2*k/3)
  p <- if (n.max > 0) sum(sapply(1:n.max, outer)) else 0
  return (p)
}


################################################################
# (Unconditional) Probability that the first pair occurs at k. #
# Hardcoded for case of suits=3 case.                          #
################################################################
first.pair.probability.with.3.suits <- function(k, ranks=13) {
  deck <- 3*ranks
  probability.of.no.pair.before.pair.at.k <- 0
  if (k==2) probability.of.no.pair.before.pair.at.k <- 1
  if (k>2) {
    p1 <- pair.probability.with.3.suits.given.joker.already.dealt(k-2, ranks-1) * (k-3)/(deck-2)
    p2 <- 1/(deck-2)
    p3 <- pair.probability.with.3.suits(k-2, ranks-1) * (deck-k)/(deck-2)
    probability.of.no.pair.before.pair.at.k <- 1-(p1+p2+p3)
  }
  probability.of.pair.at.k <- 2/(deck-1)
  return (probability.of.no.pair.before.pair.at.k * probability.of.pair.at.k)
}


###################################################################################
# Calculate theoretical probability of a pair occurring within the first k cards. #
# Hardcoded for case of suits=4 case.                                             #
###################################################################################
pair.probability.with.4.suits <- function(k, ranks=13) {
  deck <- 4*ranks
  outer <- function(n) {
    q.min <- max(0, n-floor(k/2)) # the minimum number of quartets
    q.max <- floor(n/3) # the maximum number of quartets
    innerq <- function(q) {
      d.min <- max(0, n-floor(2*k/3)-3*q) # the minimum number of doublets
      d.max <- floor((n-3*q)/2) # the maximum number of doublets
      innerd <- function(d) {
        t.min <- max(0, n-floor(2*k/3)-3*q-2*d) # the minimum number of triplets
        t.max <- floor((n-3*q-2*d)/2) # the maximum number of triplets
        innert <- function(t) {
          s <- n-3*q-2*d-2*t
          cards <- 2*s+3*t+4*d+4*q
          p <- (-1)^(n+1)
          p <- p * choose(ranks, s)
          p <- p * choose(ranks-s, t)
          p <- p * choose(ranks-s-t, d)
          p <- p * choose(ranks-s-t-d, q)
          p <- p * 12^s
          p <- p * 24^t
          p <- p * 12^d
          p <- p * 24^q
          p <- p / prod((deck-cards+1):deck)
          p <- p * prod((k-cards+1):(k-n))
          return (p)
        }
        return (sum(sapply(t.min:t.max, innert)))
      }
      return (sum(sapply(d.min:d.max, innerd)))
    }
    return (sum(sapply(q.min:q.max, innerq)))
  }
  n.max <- floor(3*k/4)
  p <- if (n.max > 0) sum(sapply(1:n.max, outer)) else 0
  return (p)
}


###################################################################################
# Calculate theoretical probability of a pair occurring within the first k cards. #
# In addition to the standard deck, 2 jokers are included (which can pair up)     #
# Hardcoded for case of suits=4 case.                                             #
###################################################################################
pair.probability.with.4.suits.and.pair.of.jokers <- function(k, ranks=13) {
  deck <- 4*ranks + 2
  outer <- function(n) {
    q.min <- max(0, n-floor(k/2)) # the minimum number of quartets
    q.max <- floor(n/3) # the maximum number of quartets
    innerq <- function(q) {
      d.min <- max(0, n-floor(2*k/3)-3*q) # the minimum number of doublets
      d.max <- floor((n-3*q)/2) # the maximum number of doublets
      innerd <- function(d) {
        t.min <- max(0, n-floor(2*k/3)-3*q-2*d) # the minimum number of triplets
        t.max <- floor((n-3*q-2*d)/2) # the maximum number of triplets
        innert <- function(t) {
          # Work out what combinations of jack and non-joker pairs are required to make up
          # the required amount of matches. If the triples, twins and quartets leave only 2 cards 
          # remaining, these must the 2 jokers. Otherwise, we can try a combination of single pairs 
          # with either 0 or 1 pair of jokers.
          remaining <- n-3*q-2*d-2*t
          jokers.min <- (remaining > 0 && 3*t+4*d+4*q+2 == deck)
          jokers.max <- (remaining > 0)
          innerj <- function(j) {
            s <- n-3*q-2*d-2*t-j
            cards <- 2*s+2*j+3*t+4*d+4*q
            p <- (-1)^(n+1)
            p <- p * choose(ranks, s)
            p <- p * choose(ranks-s, t)
            p <- p * choose(ranks-s-t, d)
            p <- p * choose(ranks-s-t-d, q)
            p <- p * 2^j
            p <- p * 12^s
            p <- p * 24^t
            p <- p * 12^d
            p <- p * 24^q
            p <- p / prod((deck-cards+1):deck)
            p <- p * prod((k-cards+1):(k-n))
            return (p)
          }
          return (sum(sapply(jokers.min:jokers.max, innerj)))
        }
        return (sum(sapply(t.min:t.max, innert)))
      }
      return (sum(sapply(d.min:d.max, innerd)))
    }
    return (sum(sapply(q.min:q.max, innerq)))
  }
  n.max <- floor(3*k/4)
  p <- if (n.max > 0) sum(sapply(1:n.max, outer)) else 0
  return (p)
}


###################################################################################
# Calculate theoretical probability of a pair occurring within the first k cards. #
# Conditioned on a joker having been dealt before the k^th card.                  #
# Hardcoded for case of suits=4 case.                                             #
###################################################################################
pair.probability.with.4.suits.given.joker.already.dealt <- function(k, ranks=13) {
  deck <- 4*ranks
  outer <- function(n) {
    q.min <- max(0, n-floor(k/2)) # the minimum number of quartets
    q.max <- floor(n/3) # the maximum number of quartets
    innerq <- function(q) {
      d.min <- max(0, n-floor(2*k/3)-3*q) # the minimum number of doublets
      d.max <- floor((n-3*q)/2) # the maximum number of doublets
      innerd <- function(d) {
        t.min <- max(0, n-floor(2*k/3)-3*q-2*d) # the minimum number of triplets
        t.max <- floor((n-3*q-2*d)/2) # the maximum number of triplets
        innert <- function(t) {
          s <- n-3*q-2*d-2*t
          cards <- 2*s+3*t+4*d+4*q
          p <- (-1)^(n+1)
          p <- p * choose(ranks, s)
          p <- p * choose(ranks-s, t)
          p <- p * choose(ranks-s-t, d)
          p <- p * choose(ranks-s-t-d, q)
          p <- p * 12^s
          p <- p * 24^t
          p <- p * 12^d
          p <- p * 24^q
          p <- p / prod((deck-cards+1):deck)
          p <- p * prod((k-cards):(k-n))
          p <- p * (k-n-1)/(k-n)/(k-1)
          return (p)
        }
        return (sum(sapply(t.min:t.max, innert)))
      }
      return (sum(sapply(d.min:d.max, innerd)))
    }
    return (sum(sapply(q.min:q.max, innerq)))
  }
  n.max <- floor(3*k/4)
  p <- if (n.max > 0) sum(sapply(1:n.max, outer)) else 0
  return (p)
}


################################################################
# (Unconditional) Probability that the first pair occurs at k. #
# Hardcoded for case of suits=4 case.                          #
################################################################
first.pair.probability.with.4.suits <- function(k, ranks=13) {
  deck <- 4*ranks
  probability.of.no.pair.before.pair.at.k <- 0
  if (k==2) probability.of.no.pair.before.pair.at.k <- 1
  if (k==3) probability.of.no.pair.before.pair.at.k <- 1-2/(deck-2)
  if (k>3) {
    p <- 2/(deck-2)
    # slightly wrong - the jokers function should exclude possibility of a trailing joker
    q <- pair.probability.with.4.suits.and.pair.of.jokers(k-2, ranks-1) * (1-p)
    probability.of.no.pair.before.pair.at.k <- 1-(p+q)
  }
  probability.of.pair.at.k <- 3/(deck-1)
  return (probability.of.no.pair.before.pair.at.k * probability.of.pair.at.k)
}

