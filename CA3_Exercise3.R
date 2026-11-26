# part 1 
df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))

# p2
nll_lm <- function(data, par){
  pred <- par[1] + par[2]*data$x1 + par[3]*data$x2 + par[4]*data$x3
  res <- data$y - pred
  llik<- dnorm(res, mean = 0, sd = par[5], log = TRUE)
  return(-sum(llik))
}

# p3
ig <- c(mean(df$y), 0, 0, 0, 1)
f <- optim(par = ig, fn = nll_lm, data=df,
           method = "L-BFGS-B",
           lower= c(-Inf, -Inf, -Inf, -Inf, 0.01),
           upper = c(Inf, Inf, Inf, Inf, Inf))
f$par

# part 5
X <- cbind(1, df$x1, df$x2, df$x3)
Y <- df$y
beta <- solve(t(X)%*% X) %*% t(X) %*% Y
beta
f$par[1:4]

#part 6 
res <- Y - X %*% beta
n <- nrow(X)
p <- ncol(X)

sigma2 <- (t(res) %*% res)/(n-p)

sigma2
f$par[5]^2

# p8
f <- optim(par = ig, fn = nll_lm, data=df,
           method = "L-BFGS-B",
           lower= c(-Inf, -Inf, -Inf, -Inf, 0.01),
           upper = c(Inf, Inf, Inf, Inf, Inf),
           hessian = TRUE)
se <- sqrt(diag(solve(f$hessian)))
se[1:4]