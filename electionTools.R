LargestRemainders <- function(n,v) {
  floats <- n*v
  wholes <- floor(floats)
  remainders <- floats - wholes
  nwhole <- sum(wholes)
  nremaining <- n-nwhole
  remseat <- rank(remainders,ties.method="random")>length(v)-nremaining
  wholes + remseat  
}

ImposeThreshold <- function(v, th) {
  vcut = v*(v >= th)
  if (sum(vcut) == 0) {
    rv <- v
  } else {
    rv <- vcut/sum(vcut)
  }
  rv
}