ls.cvrmse <- function(ls.out)
# Compute cross-validated root mean squared error of prediction.
# Handles missing values.
# ls.out is a fitted regression model from lsreg or lm.
# (c) Copyright William J. Welch 1997
# 2005.04.02: arguments x, y, intercept replaced by ls.out.
{
     res.cv <- ls.out$residuals / (1.0 - ls.diag(ls.out)$hat)

     # Identify NA's and remove them.
     is.na.res <- is.na(res.cv)
     res.cv <- res.cv[!is.na.res]

     cvrmse <- sqrt(sum(res.cv^2) / length(res.cv))
     return(cvrmse)
}

