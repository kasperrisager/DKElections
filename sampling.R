
rdirich <- function(alpha, nrep=1, seed=NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  alpha <- as.array(alpha)
  rank_alpha = length(dim(alpha))

  if (rank_alpha == 1) {
    randgamma <- sapply(alpha, function(x) rgamma(nrep,x))
  } else {
    randgamma <- apply(alpha,1:rank_alpha,function(x) rgamma(nrep,x))
  }
  dim(randgamma) <- c(nrep,dim(alpha))
  dimnames(randgamma)[2:(rank_alpha+1)] <- dimnames(alpha)
  prgamma <- aperm(randgamma,c(2,1,seq(from=3,length.out = rank_alpha-1)))
  denom <- rep(apply(prgamma,2:(rank_alpha+1),sum),each=dim(alpha)[[1]])
  prgamma / denom
  
}







