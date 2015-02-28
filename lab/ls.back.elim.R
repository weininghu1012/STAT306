ls.back.elim <- function(x, y, intercept = TRUE, alpha = 0.05,
     verbose = TRUE)
# Finds the variable with the smallest t statistic.
# If this t is not significant at level alpha (two-sided
# test) the variable is deleted from the x matrix.
# If a variable is deleted and verbose is TRUE then the
# the new regression is printed (via ls.print).
# Returns the new x matrix (which might be unchanged).
# x         - X matrix for current model (no column of 1's
#             should be included for an intercept term).
# y         - Y vector.
# intercept - Set to FALSE for a no-intercept model.
# alpha     - significance level for the t test to remove.
# verbose   - new regression is printed if verbose is TRUE.
#
# 2000.03.15: round applied to p.val when no variable deleted.
# 2000.04.18: alpha output in cat() (twice).
# 2002.12.18: "smallest" changed to "largest" 
#             in "No variable deleted ..." message.
{
     # Compute absolute t statistics.
     ls.out <- lsfit(x, y, intercept = intercept)
     diag <- ls.diag(ls.out)
     t.stat <- abs(ls.out$coef / diag$std.err)

     # If present, exclude the intercept from removal.
     if (intercept == TRUE)
          t.stat <- t.stat[-1]

     # order.t[1] is the index of the smallest t statistic, etc.
     order.t <- order(t.stat)

     # Column in x with the minimum t statistic.
     col.t.min <- order.t[1]

     # The minimum t statistic.
     t.min <- t.stat[col.t.min]

     # Degrees of freedom for residual, allowing for NA's.
     df.res <- sum(!is.na(ls.out$residuals)) - ncol(x)
     if (intercept)
          df.res <- df.res - 1

     # P-value for minimum t.
     p.val <- 2 * (1 - pt(t.min, df.res))

     if (p.val >= alpha)
     {
          cat("Deleting", dimnames(x)[[2]][col.t.min],
                    "with p-value", round(p.val, 4), ">", alpha, "\n")
          x <- x[,-col.t.min]
          if (verbose)
               ls.print(lsfit(x, y, intercept = intercept))
     }
     else
          cat("No variable deleted (largest p-value is ",
                    round(p.val, 4), " <= ", alpha, ")\n", sep = "")

     # Return the x matrix.
     return(x)
}

