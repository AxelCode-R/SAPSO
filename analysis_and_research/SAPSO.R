mrunif <- function(nr, nc, lower, upper) {
  return(matrix(runif(nr*nc,0,1),nrow=nr,ncol=nc)*(upper-lower)+lower)
}

SAPSO <- function(
  ofn,
  cfn,
  pos,
  s = 50,
  time = NULL,
  iter = 1000,
  lower = 0,
  upper = 1,
  print_info = FALSE,
  save_stats = FALSE
){
  # ofn = function(x){sum(x)}
  # cfn = function(x){abs(sum(x^2)-1)}
  # pos = rep(NA,10)
  # lower = 0
  # upper = 1
  # control <- list(
  #   delta = 0.001,
  #   s = 5,
  #   w = 1,
  #   cp = 1,
  #   cg = 1,
  #   time = NULL,
  #   iter = 1000,
  #   print_info = FALSE,
  #   save_stats = FALSE
  # )


  # length of dimension
  d <- length(pos)
  if(length(lower) == 1){lower <- rep(lower, d)}
  if(length(upper) == 1){upper <- rep(upper, d)}

  X <- mrunif(nr = d, nc = control$s, lower = lower, upper = upper)
  if(is.numeric(pos)){X[,1] <- pos}
  V <- mrunif(nr = d, nc = control$s, lower = lower, upper = upper) # has to be fixed

  Xc <- apply(X, 2, cfn)
  Xo <- sapply(1:control$s, function(x){if(Xc[x] <= control$delta){ofn(X[x])}else{NA}})

  Pp <- X
  Po <- Xo
  Pc <- Xc

  g_pos <- if(!any(Pc <= control$delta)){
    which.min(Pc)
  }else{
    which(Pc <= control$delta)[which.min(Po[,Pc <= control$delta])]
  }
  Ppg <- Pp[,g_pos]
  Pog <- Po[g_pos]
  Pcg <- Pc[g_pos]


  i <- 1
  while(i <= control$iter){


  }



}










