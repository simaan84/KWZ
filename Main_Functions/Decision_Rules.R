library(zipfR) # needed for one of the decision rules
library(RiskPortfolios) # used for covariance matrix shrinkage

## --------------------------------------------------------
U <- function(X,Mu,Sigma,gamma) {
  u1 <- t(X)%*%(Mu)
  u2 <- t(X)%*%Sigma%*%X
  total <- u1 - (gamma/2)*u2
  return(c(total))
}

G <- function(X,Mu,Sigma,gamma) {
  total <-  Mu - gamma*Sigma%*%X
  return(total)
}

U2 <- function(X,Sigma) {
  u2 <- t(X)%*%Sigma%*%X
  return(c(u2))
}


G2 <- function(X,Sigma) {
  g2 <- Sigma%*%X
  total <- g2
  return(total)
}

# add constraints
BC_f <- function(d) {
  # sum to one constraint
  A <- matrix(1,1,d)
  A <- rbind(A,-A)
  B <- c(0.99999,-1.00001)
  
  # short-sales constraints
  A2 <- diag(rep(1,d))
  B2 <- rep(0,d)
  A2 <- rbind(A,A2)
  B2 <- c(B,B2)
  
  # stack altogether in a list
  BC1 <- list(A,B)
  BC2 <- list(A2,B2)
  list(BC1,BC2)
}


## --------------------------------------------------------
d <- 10
A <- BC_f(d)[[2]][[1]]
b <- BC_f(d)[[2]][[2]]
x <- rep(1/d,d)
all(A%*%x >= b)


## --------------------------------------------------------
DR_function <- function(R_sub,gamma,sample_size,TC) {
  
  # takes data  object R_sub - see below
  R_sub2 <- tail(R_sub,sample_size)
  R_sub2 <- R_sub2[,-1]
  d <- ncol(R_sub2)
  Mu <- apply(R_sub2,2,mean)
  Sigma <- var(R_sub2)*(d-1)/d # for MLE estimate divide by d
  Sig_inv <- solve(Sigma)
  e <- rep(1,d)
  
  # first portfolio is the global minimum variance portfolio. KWZ denote it by $\hat{w}_{g,t}$
  GMV <- Sig_inv%*%e/sum(Sig_inv)
  B_mat <- Sig_inv%*%( diag(d) - e%*%t(GMV)  )
  # the second is the plug-in portfolio. KWZ denote it by $\hat{w}_{z,t}$
  MV <- GMV + (1/gamma)*B_mat%*%Mu
  
  # the unbiased estimate
  MV_UB <- GMV + (sample_size - d - 1)/(gamma*sample_size)*B_mat%*%Mu
  
  # Naive Portfolio
  Naive <- rep(1/d,d)
  
  BC <- BC_f(d)[[2]] # 2 for no short-sales and 1 for yes
  A <- BC[[1]]
  B <- BC[[2]]
  
  # initial guess is based on naive portfolio
  X0 <- Naive
  X_opt <- constrOptim(X0,
                       function(x) -U(x,Mu,Sigma,gamma),
                       grad = function(x) -G(x,Mu,Sigma,gamma),
                       ui = A,ci = B)
  X1 <- X_opt$par
  MV_NS <- X1/sum(X1)
  
  X_opt <- constrOptim(X0,function(x) U2(x,Sigma),
                       grad = function(x) G2(x,Sigma),
                       ui = A,ci = B)
  X1 <- X_opt$par
  GMV_NS <- X1/sum(X1)
  
  # Volatility Timing: the first strategy proposed by Kirby and Ostdiek (2012)
  KO_VT <- (1/diag(Sigma))^4 # footnote in Table 2 states that eta = 4
  KO_VT <- KO_VT/sum(KO_VT)
  
  # Reward-to-Risk Timing: The second strategy proposed by Kirby and Ostdiek (2012)
  Mu_plus <- Mu
  Mu_plus[Mu_plus < 0] <- 0
  KO_RT <- (Mu_plus/diag(Sigma))^4 # footnote in Table 2 states that eta = 4
  KO_RT <- KO_RT/sum(KO_RT)
  
  ### add KWZ decision rule
  N <- d
  T <- sample_size
  # the proposed way to estimate
  psi2hat <- t(MV) %*% Mu;
  psi2hat <- ((T-N-1)*psi2hat-(N-1))/T +
    ((2*(psi2hat)^((N-1)/2)*((1+psi2hat)^(-(T-2)/2)))/(T*Ibeta(psi2hat/(1+psi2hat),(N-1)/2,(T-N+1)/2)));
  psi2hat <- as.numeric(psi2hat)
  
  kappaE <- (((T-N)*(T-N-3))/(T*(T-2)))*(psi2hat/(psi2hat+(N-1)/T));
  KWZ <-  GMV*(1-kappaE) + MV*(kappaE)
  
  # Ledoit and Wolf (2004)
  m_i <- list(type = "diag")
  Sigma <- covEstimation(as.matrix(R_sub2),control = m_i)
  Sig_inv <- solve(Sigma)
  e <- rep(1,d)
  
  # update the GMV based on the cov estimate
  GMV_LW_2004 <- Sig_inv%*%e/sum(Sig_inv)
  B_mat <- Sig_inv%*%( diag(d) - e%*%t(GMV_LW_2004))
  MV_LW_2004 <- GMV_LW_2004 + (1/gamma)*B_mat%*%Mu
  
  # do the same for the KWZ decision rule
  KWZ_LW_2004 <-  GMV_LW_2004*(1-kappaE) + MV_LW_2004*(kappaE)
  
  # organize in a similar order as in KWZ
  list(KWZ = KWZ, MV = MV, MV_UB = MV_UB,
       KWZ_LW_2004 = KWZ_LW_2004, MV_LW_2004 = MV_LW_2004,
       MV_NS = MV_NS,
       GMV = GMV,GMV_NS = GMV_NS,
       Naive = Naive,
       KO_VT = KO_VT,KO_RT = KO_RT)
}

