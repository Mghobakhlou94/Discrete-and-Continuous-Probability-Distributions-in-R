
rgenerator <- function(){
  c <- sample(2,20,replace = TRUE)
  random = 0
  for(i in 1:20){
    if(c[i] == 1){
      random <- random + 2 ** (i-1)
    }
  }
  return(random)
}


dugen <- function(a,b){
  temp <- rgenerator()/(2 ** 20)
  temp <- temp * (b - a)
  temp <- temp + a
  return(temp)
}


cugen <- function(){
  return(dugen(0,1))
}


brgen <- function(p){
  s <- cugen()
  if(s < p){
    return(1)
  }
  return(0)
}

bigen <- function(n,p){
  x <- 0
  for(i in 1:n){
    x <- x + brgen(p)
  }
  return(x)
}


gegen <- function(p){
  cntr <- 0
  while(TRUE){
    temp <- brgen(p)
    cntr <- cntr + 1
    if(temp == 1){
      break
    }
  }
  return(cntr)
}

expgen <- function(l){
  x <- cugen()
  t <- (-1/l) * log(x)
  return(t)
}

gagen <- function(l,k){
  t <- 0
  for(i in 1:k){
    t <- t + expgen(l)
  }
  return(t)
}



pogen <- function(param, t){
  k <- c()
  i <- 1
  res <- 0
  while(res <= t){
    k[i] <- expgen(param)
    res <- sum(k)
    i <- i+1
  }
  result <- i-2
  return(result)
}


nogen <- function(u, s){
  temp <- pogen(u, s)
  standard <- (temp - u)/sqrt(u)
  return (standard)
}
