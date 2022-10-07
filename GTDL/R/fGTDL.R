#'@name fGTDL
#'@aliases fGTDL dGTDL hGTDL sGTDL rGTDL fires
#'
#'@title The GTDL distribution
#'
#'@description Density function, survival function, failure function and random 
#'generation for the GTDL distribution.
#'@param t vector of integer positive quantile. 
#'@param param parameters (alpha and gamma are scalars, lambda non-negative).
#'@param n number of observations. 
#'@param log logical; if TRUE, probabilities p are given as log(p).
#'
#'@references
#'
#'\itemize{
#' \item Mackenzie, G. (1996). Regression Models for Survival Data: The Generalized Time-Dependent Logistic Family. Journal of the Royal Statistical Society. 
#' Series D (The Statistician). 45. 21-34. 
#'}
#'
#'@return \code{dGTDL} gives the density function, \code{hGTDL} gives the failure function, \code{sGTDL} gives the survival function and \code{rGTDL} generates random samples.
#'@return Invalid arguments will return an error message.
#'
#'@source [d-p-q-r]GTDL are calculated directly from the definitions. 
#'
#'@details
#'\itemize{
#'\item Density function
#'\deqn{f(t\mid \boldsymbol{\theta})=\lambda\left(\frac{\exp\{\alpha{t}+\boldsymbol{X}^{\top}\boldsymbol{\beta}\}}{1+\exp\{\alpha{t}+\boldsymbol{X}^{\top}\boldsymbol{\beta}\}}\right)\times\left(\frac{1+\exp\{\alpha{t}+\boldsymbol{X}^{\top}\boldsymbol{\beta}\}}{1+\exp\{\boldsymbol{X}^{\top}\boldsymbol{\beta}\}}\right)^{-\lambda/\alpha}}
#'
#'\item Survival function
#'\deqn{S(t \mid \boldsymbol{\theta})=\left(\frac{1+\exp\{\alpha{t}+\boldsymbol{X}^{\top}\boldsymbol{\beta}\}}{1+\exp\{\boldsymbol{X}^{\top}\boldsymbol{\beta}\}}\right)^{-\lambda/\alpha}}
#' 
#'\item Failure function
#'\deqn{h(t\mid\boldsymbol{\theta})=\lambda\left(\frac{\exp\{\alpha{t}+\boldsymbol{X}^{\top}\boldsymbol{\beta}\}}{1+\exp\{\alpha{t}+\boldsymbol{X}^{\top}\boldsymbol{\beta}\}}\right)}
#' 
#' }
#' 
#'@examples
#'
#'### Example 1
#'
#'library(GTDL)
#'library(ggplot2)
#'library(tidyverse)
#'
#'t <- seq(0, 20, by = 0.01)
#'theta <- c(1.00, -0.05, -1.00)
#'y1 <- hGTDL(t, theta)
#'y2 <- sGTDL(t, theta)
#'y3 <- dGTDL(t, theta)
#'
#'df <- data.frame(t, y1, y2, y3)
#'
#'df %>% ggplot() + geom_line(aes(t,y = y1, col = 'failure function'), linetype = 1) +
#'geom_line(aes(t, y = y2, col = 'survival function'), linetype = 2 ) +
#'geom_line(aes(t, y = y3, col = 'density function'), linetype = 3)+
#'theme(legend.position = "right", plot.title = element_text(size = rel(1.2), lineheight = .9, family = "Calibri", face = "bold", colour = "brown"))
#'
#'
#'### Example 2
#'
#'library(GTDL)
#'library(ggplot2)
#'library(tidyverse)
#'
#'t <- seq(0, 20, by = 0.01)
#'theta1 <- c(1.00, 0.50, -1.00)
#'theta2 <- c(1.00, -0.50, 1.00)
#'theta3 <- c(1.00, -0.25, 1.00)
#'theta4 <- c(1.00, -0.06, -1.60)
#'theta5 <- c(1.00, 0.25, -1.00)
#'
#'y1 <- hGTDL(t, theta1)
#'y2 <- hGTDL(t, theta2)
#'y3 <- hGTDL(t, theta3)
#'y4 <- hGTDL(t, theta4)
#'y5 <- hGTDL(t, theta5)
#'
#'df <- data.frame(t, y1, y2, y3, y4, y5)
#'
#'df %>% ggplot() + geom_line(aes(t, y = y1), linetype = 2) + 
#'  geom_line(aes(t, y = y2), linetype = 3) + geom_line(aes(t, y = y3), linetype = 4) + 
#'  geom_line(aes(t, y = y4), linetype = 5) + geom_line(aes(t, y = y5), linetype = 6) + 
#'  labs(title = "", y = "")

#'@rdname fGTDL 
#'@export

dGTDL<-function(t,param,log = FALSE){
  lambda <- param[1]
  alpha <- param[2]
  gamma <- param [3]
  d1 <- ((lambda*exp(t*alpha+gamma))/(1+exp(t*alpha+gamma)))
  d2 <- ((1+exp(t*alpha+gamma))/(1+exp(gamma)))^(-lambda/alpha)
  
  if(log == FALSE){
    return(d1*d2)
  }
  else {
    return(log(d1*d2))
    }
  }

#'@rdname fGTDL 
#'@export

hGTDL <- function(t,param){
  lambda <- param[1]
  alpha <- param[2]
  gamma <- param[3]
  h1 <- (lambda*exp(t*alpha+gamma))
  h2 <- (1+exp(t*alpha+gamma))
  return(h1/h2)
}

#'@rdname fGTDL 
#'@export

sGTDL <- function(t,param){
  lambda <- param[1]
  alpha <- param[2]
  gamma <- param[3]
  return(dGTDL(t,param)/hGTDL(t,param))
}

#'@rdname fGTDL
#'@export

rGTDL <- function(n,param){
  lambda <- param[1]
  alpha <- param[2]
  gamma <- param[3]
  u <- runif(n)
  t <- (1/alpha)*(log((1+exp(gamma))*(1-u)^(-alpha/lambda)-1)-gamma)
  return(t)
}
