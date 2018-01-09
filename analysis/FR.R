## code to estimate a funtional response
## I am trying here to fit to percent in diet, which will not produce the results I am looking for
## look at my MSc thesis

## function funct respose, if delta = 1 then it is type II, if delta = 2 then it is type III
funcresponse <- function(x, a, h, delta) {(a*(x^delta))/(1 + a*h*(x^delta))}

obj.f.FR <- function(dat, pars) {
  y <- dat[,1]
  x <- dat[,2]
  a <- pars[1]
  h <- pars[2]
  delta <- pars[3]
  obj <- sum(y - ((a*(x^delta))/(1 + a*h*(x^delta)) )^ 2)
  return(obj)
}

delta <- 1
fit <- optim(par = c(1, 1, delta),
             dat = subset(ah, nafo == '3L', select = c(percbio, total.cpue)),
             fn = obj.f.FR,
             method = c("Nelder-Mead"),
             hessian = F)#,
#      lower = c(0.0002, 0.0002, delta),
#     upper = c(10, 10, delta))


Efr <- funcresponse(subset(ah, nafo == '3L', select = c(total.cpue)), fit$par[1], fit$par[2], fit$par[3])
