# Calculate theoretical probability of a pair occurring within the first k cards.
# Hardcoded for case of suits=2 case.
pair.probability.with.2.suits <- function(k, ranks=13, with.rogue=FALSE) {
  f <- function(s) {
    p <- (-1)^(s+1)
    p <- p * choose(ranks, s)
    p <- p * 2^s
    p <- p * factorial(2*ranks-2*s) / factorial(2*ranks)
    p <- p * factorial(k-s) / factorial(k-2*s)
    if (with.rogue) p <- p * (1-2*s/k)
    return (p)
  }
  max <- floor(k/2)
  p <- if (max > 0) sum(sapply(1:max, f)) else 0
  if (with.rogue) {
    weight <- k/(2*ranks+1)
    p <- p * weight + pair.probability.with.2.suits(k, ranks, with.rogue=FALSE) * (1-weight)
  }
  return (p)
}


# Calculate theoretical probability of a pair occurring within the first k cards.
# Hardcoded for case of suits=3 case.
pair.probability.with.3.suits <- function(k, ranks=13, with.rogue=FALSE) {
  outer <- function(n) {
    t.min <- max(0, n-ranks) # the minimum number of triples
    t.max <- floor(n/2) # the maximum number of triples
    inner <- function(t) {
      q <- (-1)^(n+1)
      q <- q * choose(ranks, n-2*t)
      q <- q * choose(ranks-n+2*t, t)
      q <- q * 6^(n-t)
      q <- q / prod((3*ranks+t-2*n+1):(3*ranks))
      q <- q * prod((k+t-2*n+1):(k-n))
      if (with.rogue) q <- q * (1-(2*n-t)/k)
      return (q)
    }
    return (sum(sapply(t.min:t.max, inner)))
  }
  n.max <- floor(2*k/3)
  p <- if (n.max > 0) sum(sapply(1:n.max, outer)) else 0
  if (with.rogue) {
    weight <- k/(3*ranks+1)
    p <- p * weight + pair.probability.with.3.suits(k, ranks, with.rogue=FALSE) * (1-weight)
  }
  return (p)
}


# Calculate theoretical probability of a pair occurring within the first k cards.
# Hardcoded for case of suits=4 case.
pair.probability.with.4.suits <- function(k, ranks=13) {
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
          p <- p / prod((4*ranks-cards+1):(4*ranks))
          p <- p * factorial(k-n) / factorial(k-cards)
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


# (Unconditional) Probability that the first pair occurs at k.
# Hardcoded for case of suits=2 case.
# BROKEN!!!
first.pair.probability.with.2.suits <- function(k, ranks=13) {
  return ((1 - pair.probability.with.2.suits(k-2, ranks-1))/(2*ranks-1))
}
