library(derivmkts)
#############################################
# Binomial tree pricer for American options
#############################################

p_prob <-function(r, delta_t, sigma) {
  u = exp(sigma*sqrt(delta_t))
  d = exp(-sigma*sqrt(delta_t))
  return((exp(r*delta_t) -d)/(u-d))
  }

tree_stock <-function(S, sigma, delta_t, N) {
  tree = matrix(0, nrow=N+1, ncol=N+1)
  U = exp(sigma*sqrt(delta_t))
  D = exp(-sigma*sqrt(delta_t))
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i, j] = S * U^(j-1) * D^((i-1)-(j-1))
    }  }
  return(tree)
  }

value_binomial_option <-function(tree, sigma, delta_t, r, K, type,american ){
  q = p_prob(r, delta_t, sigma)
  option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  if(type == "call") {
    option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),]-K,0)
    }
  else {
    option_tree[nrow(option_tree),] = pmax(K-tree[nrow(tree),],0)
    }
  if (american == TRUE) {
    for (i in (nrow(tree)-1):1) {
      for(j in 1:i) {
        exercise.payoff<-if (type == "call") max(tree[i,j] -K, 0) else max(K -tree[i,j], 0)
        hold.payoff <-(q*option_tree[i+1,j+1] + (1-q)*option_tree[i+1,j])/exp(r*delta_t)
        option_tree[i,j] <-max(exercise.payoff, hold.payoff)
      }
    }
    return(option_tree)
  }
  else {
    for (i in (nrow(tree)-1):1) {
      for(j in 1:i) {
        option_tree[i,j] = (p*option_tree[i+1,j+1] + (1-p)*option_tree[i+1,j])/exp(r*delta_t)
      }
    }
    return(option_tree)
  }
}

binomial_option<-function(type, sigma, T, r, K, S, N,american) {
  q <-p_prob(r=r, delta_t=T/N, sigma=sigma)
  tree <-tree_stock(S=S, sigma=sigma, delta_t=T/N, N=N)
  option <-value_binomial_option(tree, sigma=sigma, delta_t=T/N, r=r, K=K, type=type,american)
  return(list(q=q, stock=tree, option=option, price=option[1,1]))
}

BSoptionprice <- function(cp,S0,K,T,sigma,r){
  d1<-(log(S0/K)+(r+0.5*sigma^2)*T)/(sigma*sqrt(T))  
  d2<-d1-sigma*sqrt(T)
  ifelse(cp=="put",
         return(-S0*pnorm(-d1)+exp(-r*T)*K*pnorm(-d2)),
         return(S0*pnorm(d1)-exp(-r*T)*K*pnorm(d2)))
}

BSoptionDelta<-function(cp,S0,K,T,sigma,r){
  d1<-(log(S0/K)+(r+0.5*sigma^2)*T)/(sigma*sqrt(T))  
  ifelse(cp=="put",
         return(pnorm(-d1)),
         return(pnorm(d1)))
}

# pnorm(x) = N(x), dnorm(x) = N'(x)

BSoptionGamma<-function(cp,S0,K,T,sigma,r){
  d1<-(log(S0/K)+(r+0.5*sigma^2)*T)/(sigma*sqrt(T))  
  return(dnorm(d1)/(S0*sigma*sqrt(T)))
}

BSoptionTheta<-function(cp,S0,K,T,sigma,r){
  d1<-(log(S0/K)+(r+0.5*sigma^2)*T)/(sigma*sqrt(T))  
  d2<-d1-sigma*sqrt(T)
  term1 <- -S0*dnorm(d1)*sigma/(2*sqrt(T))
  term2 = r*K*exp(-r*T)*pnorm(d2)
  return(term1 - term2)
}


BSoptionprice("call",50,50,5/12,0.4,0.1)
BSoptionDelta("call",50,50,5/12,0.4,0.1)
BSoptionGamma("call",50,50,5/12,0.4,0.1)
BSoptionTheta("call",50,50,5/12,0.4,0.1)

# uses derivmkts ######
#s=40; k=40; v=0.30; r=0.08; tt=0.25; d=0;
#greeks(bscall(s, k, v, r, tt, d), complete=FALSE, long=FALSE, initcaps=TRUE)
#greeks2(bscall, list(s=s, k=k, v=v, r=r, tt=tt, d=d))
#greeks2(bscall, list(s=s, k=k, v=v, r=r, tt=tt, d=d))[c('Delta', 'Gamma'), ]
#bsopt(s, k, v, r, tt, d)
#bsopt(s, c(35, 40, 45), v, r, tt, d)
#bsopt(s, c(35, 40, 45), v, r, tt, d)[['Call']][c('Delta', 'Gamma'), ]

