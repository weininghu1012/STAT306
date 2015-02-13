#webwork 3
# question1, read the data in
calories=c(100, 110, 100, 100, 100, 120, 110, 150, 120, 110,
           100, 70, 110, 100, 150, 110, 130, 110, 110, 90, 100, 80, 110,
           100, 90, 110, 90, 110, 140, 110, 90, 110, 90, 110, 110, 110,
           110, 110, 100, 120, 120, 110)

#explanatory variable
protein=c(3, 1, 2, 3, 3, 3, 1, 4, 3, 2, 2, 4, 3, 3, 4, 3, 3, 1, 2, 3, 3,
          2, 2, 2, 2, 2, 3, 6, 3, 1, 3, 3, 2, 1, 1, 2, 2, 1, 2, 1, 3, 2)
fat=c(1, 1, 0, 1, 1, 3, 1, 3, 2, 2, 0, 1, 0, 1, 3, 2, 2, 0, 1, 0, 2, 0, 1,
      1, 0, 1, 0, 2, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 3, 1, 0)
fiber=c(3, 0, 1, 3, 3, 3, 0, 3, 5, 1.5, 0, 10, 3, 3, 3, 2, 1.5, 0, 0, 3,
        2.5, 3, 1, 2, 3, 0, 4, 2, 4, 0, 5, 1.5, 4, 0, 0, 0, 0, 0, 2, 0, 6, 0)
carbo=c(15, 13, 18, 17, 16, 13, 12, 16, 12, 10.5, 11, 5, 17, 17,
        16, 13, 13.5, 14, 21, 20, 10.5, 16, 16, 15, 15, 21, 19, 17, 15, 13,
        13, 11.5, 15, 12, 23, 21, 12, 15, 11, 13, 11, 22)
sugars=c(5, 12, 5, 3, 3, 4, 13, 11, 10, 10, 15, 6, 3, 3, 11, 7, 10,
         11, 3, 0, 8, 0, 8, 6, 5, 3, 0, 1, 14, 12, 5, 10, 6, 13, 2, 3, 12, 9, 10,
         9, 14, 3)
mfr=c('P', 'P', 'R', 'G', 'G', 'P', 'G', 'R', 'P', 'G', 'P', 'N',
      'P', 'R', 'R', 'G', 'G', 'P', 'G', 'N', 'G', 'N', 'G', 'G', 'N', 'G',
      'N', 'G', 'G', 'G', 'P', 'G', 'R', 'G', 'R', 'G', 'G', 'G', 'G', 'G',
      'P', 'R')

# create a data frame
cereal = data.frame(cbind(calories,protein,fat,fiber,carbo,sugars))
cereal$mfr = mfr
levels(cereal$mfr)
print(table(cereal$mfr))
#categorical variable with 4 manufactures
n = nrow(cereal)
subreg = rep("other",n)
#The estimate of the signed distance of the hyperplane for manufacturer
#G relative to P is and its SE is
ireg1 = (cereal$mfr == "P")
#The estimate of the signed distance of the hyperplane for manufacturer
#N relative to R is and its SE is
ireg2 = (cereal$mfr == "R")
subreg[ireg1] = "P"
subreg[ireg2] = "R"
cereal$subreg = subreg

# to make P the baseline
subregp = C(factor(subreg),contr.treatment(3,base = 2))
subreg
