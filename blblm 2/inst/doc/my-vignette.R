## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
#  

## -----------------------------------------------------------------------------
#  library(blblm)
#  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
#  coef(fit)
#  
#  confint(fit, c("wt", "hp"))
#  
#  sigma(fit)
#  
#  sigma(fit, confidence = TRUE)
#  
#  predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
#  
#  predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)
#  

## -----------------------------------------------------------------------------
#  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
#  coef(fit)

## -----------------------------------------------------------------------------
#  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
#  coef(fit)

## -----------------------------------------------------------------------------
#  fit <- blbglm(cyl ~ wt * hp, family = gaussian, data = mtcars, m = 3, B = 100)
#  coef(fit)

## -----------------------------------------------------------------------------
#  fit <- blbglm(cyl ~ wt * hp, family = gaussian, data = mtcars, m = 3, B = 100, parallel = TRUE)
#  coef(fit)

