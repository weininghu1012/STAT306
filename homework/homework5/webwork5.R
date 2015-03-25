#webwork9
p30 = exp(2-0.12*30)/(1+exp(2-0.12*30))
p30
a=log(0.6/0.4)
(2-a)/0.12

#Question3
itrain=c(198, 187, 178, 12, 84, 156, 5, 142, 195, 162, 44, 120, 155, 146, 18, 165, 100, 85, 87, 160, 180, 8, 122, 3, 151, 65, 116, 190, 106, 54, 127, 145, 121, 91, 98, 193, 72, 43, 62, 30, 135, 83, 150, 42, 158, 164, 50, 38, 196, 23, 141, 115, 189, 47, 74, 170, 27, 60, 24, 134, 159, 52, 143, 63, 152, 89, 73, 76, 129, 136, 78, 41, 32, 133, 10, 157, 131, 119, 46, 148, 132, 174, 37, 55, 29, 184, 7, 70, 192, 20, 188, 96, 181, 168, 25, 53, 117, 59, 173, 14, 69, 95, 111, 19, 183, 21, 102, 140, 61, 149, 22, 163, 82, 200, 153, 81, 176, 9, 28, 36, 118, 94, 110, 58, 101, 171, 31, 137, 167, 185, 103, 90, 108, 68, 2, 66, 39, 109, 35, 113, 40, 97, 79, 13, 107, 6, 114, 16, 34, 191, 105, 80, 144, 161, 166, 154, 77, 128, 92, 126, 93, 125, 1, 124, 86, 17, 179, 45, 4, 99) 
ihold=c(63, 82, 100, 300, 91, 126, 170, 223, 127, 208, 279, 222, 167, 166, 324, 162, 31, 27, 246, 36, 175, 239, 156, 42, 169, 291, 25, 104, 203, 76, 110, 280, 13, 319, 306, 39, 235, 157, 5, 61, 237, 328, 234, 243, 244, 165, 6, 155, 186, 196, 153, 48, 122, 17, 2, 81, 77, 266, 15, 124, 32, 105, 218, 70, 99, 322, 307, 98, 152, 92, 317, 160, 35, 216, 128, 119, 136, 73, 227, 231, 258, 316, 79, 294, 90, 89, 285, 3, 310, 327, 185, 34, 54, 202, 83, 107, 46, 143, 241, 318, 109, 240, 30, 260, 304, 44, 259, 199, 255, 254, 1, 247, 52, 177, 184, 276, 59, 286, 189, 134, 330, 256, 20, 117, 85, 329, 135, 120, 201, 181, 148, 87, 290, 242, 129, 28, 197, 101, 236, 118, 164, 29, 19, 277, 296, 198, 50, 67, 207, 138, 64, 253, 187, 72, 75, 315, 140, 263, 78, 55, 114, 43, 123, 37, 269, 125, 47, 332, 26, 146, 228, 272, 22, 252, 226, 325, 293, 219, 159, 265, 250, 154, 232, 220, 8, 40, 188, 93, 215, 204, 301, 248, 132, 283, 16, 141, 84, 173, 297, 320) 
library(MASS) 
data(Pima.tr) 
mytrain=Pima.tr[itrain,] 
data(Pima.te) 
myhold=Pima.te[ihold,] 
# Next do the following: 
# (1) Fit the logistic regression model with all 7 
  #explanatory variables npreg, glu, bp, skin, bmi, ped, age. Call this model 1. 
model1 = glm(type~.,data = mytrain, family = "binomial")  
  
# (2) Fit the logistic regression model with 4 
# explanatory variables glu, bmi, ped, age 
# (this is best model from backward elimination 
# if all cases of Pima.tr is used). For this model with 
#4 explanatory variables, call it model 2. 
model2 = glm(type~glu+bmi+ped+age,data = mytrain,family = "binomial")
# (3) Apply both models 1 and 2 to the holdout data set and get 
# the predicted probabilities. Classify a case as diabetes if the 
# predicted probability exceeds 0.5 and otherwise classify it as non-diabetes .

pred1 = predict(model1, newdata = myhold,type = "response")
pred2 = predict(model2, newdata = myhold, type = "response")

pt1 = table(myhold$type, as.numeric(pred1>0.5))
pt2 = table(myhold$type, as.numeric(pred2>0.5))


# (4) For models 1 and 2, get the total number of misclassifications. 
# Which model is better based on this criterion? 
misc1 = pt1[1,2]+pt1[2,1]
misc2 = pt2[1,2]+pt2[2,1]
# misc1 = 37
# misc2 = 35


# (5) For models 1 and 2, compare the misclassification tables if one 
# classifies a case as diabetes if the predicted probability exceeds 0.3 
# and otherwise classify it as non-diabetes . Which is the better boundary to use?
pt11 = table(myhold$type, as.numeric(pred1>0.3))
pt21 = table(myhold$type, as.numeric(pred2>0.3))
misc11 = pt11[1,2]+pt11[2,1]
misc21 = pt21[1,2]+pt21[2,1]
# misc11 = 39
# misc21 = 40

# question c
pc1 = predict(model1, newdata = myhold[1,], type = "response")
# pc1 = 0.04825122 
pc2 = predict(model2, newdata = myhold[1,], type = "response")
# pc2 = 0.06380234
summary(model1)
summary(model2)
