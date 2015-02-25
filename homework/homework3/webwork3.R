#webwork 3
options(digits = 6)
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
cereal
#give a summary of the categories
print(table(cereal$mfr))
#categorical variable with 4 manufactures
n = nrow(cereal)
n

#fill in "N" and "R" as contrast
submfr = rep("other",n)
imfr3 = (cereal$mfr == "N")
imfr4 = (cereal$mfr == "R")
submfr[imfr3] = "N"
submfr[imfr4] = "R"
cereal$submfr = submfr
factor(submfr)
# [1] other other R     other other other other R     other other other N     other R     R     other
# [17] other other other N     other N     other other N     other N     other other other other other
# [33] R     other R     other other other other other other R    
# Levels: N other R
submfrr = C(factor(submfr),contr.treatment(3,base = 1))
calnr = lm(calories~protein+fat+fiber+carbo+sugars+submfrr)
print(summary(calnr))
residr = sqrt(sum(calnr$resid^2)/calnr$df.resid)
residr
#fill in all the categories as "other"
submfr = rep("other",n)
submfr
#The estimate of the signed distance of the hyperplane for manufacturer
#G relative to P is and its SE is
#fill in "P" and "G" as contrast
imfr1 = (cereal$mfr == "P")
imfr2 = (cereal$mfr == "G")
imfr1
# > imfr1
# [1]  TRUE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE
# [20] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [39] FALSE FALSE  TRUE FALSE
#fill in "P" if it's true
submfr[imfr1] = "P"
submfr[imfr2] = "G"
submfr
cereal$submfr = submfr
factor(submfr)
# [1] P     P     other G     G     P     G     other P     G     P     other P     other other G    
# [17] G     P     G     other G     other G     G     other G     other G     G     G     P     G    
# [33] other G     other G     G     G     G     G     P     other
# Levels: G other P
# to make P the baseline
submfrp = C(factor(submfr),contr.treatment(3,base = 1))
cereal$submfrp = submfrp
attach(cereal)  # attach data frame so that variables can be access without $
calg = lm(calories~protein+fat+fiber+carbo+sugars+submfr)
print(summary(calg))

calgp = lm(calories~protein+fat+fiber+carbo+sugars+submfrp)
print(summary(calgp))
residp = sqrt(sum(calgp$resid^2)/fit$df.resid)
residp




detach(cereal)
cereal
reg6 = lm(calories~+protein+fat+fiber+carbo+sugars+mfr)
print(summary(reg6))



#question2
# Assume s is a sample covariance or sample correlation matrix
# with (possible) row and column names, this function outputs
# partial correlation of first two variables given the rest.
pcor=function(s)
{ i=1; j=2
  i1=c(i, j)
  i2=1:nrow(s); i2=i2[c(-i, -j)]
  s11=s[i1,i1]; s12=s[i1,i2]; s21=s[i2,i1]; s22=s[i2,i2];
  condcov = s11 - s12 %*% solve(s22) %*% s21
  condcov[1,2]/sqrt(condcov[1,1] * condcov[2,2])
}
x1=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
x2=c(2, 6, 5, 4, 8, 0, 3, 1, 7, 9)
x3=c(2, 0, 7, 1, 5, 6, 3, 9, 8, 4)
y=c(4, 2, 2, 4, 10, 6, 6, 9, 7, 4)
data = data.frame(cbind(x1,x2,x3,y))
rmat = cor(data)
print (rmat)
rmat[1,1]

#caculate the part for Ryx1:x2
rsub1 = rmat[c(4,1,2),c(4,1,2)]
pcor(rsub1)

#caculate the part for Ryx2;x1x3
rsub2 = rmat[c(4,2,1,3),c(4,2,1,3)]
pcor(rsub2)
