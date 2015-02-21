# Multiple regression: Burnaby condo data with categorical explanatory variable
b=read.table("/Users/apple/Desktop/STAT306/lecturenote/burnabycondo.txt",header=T,skip=2) # data frame
print(names(b))
# [1] "MLS"      "askprice" "ffarea"   "beds"     "baths"    "floor"   
# [7] "view"     "age"      "mfee"     "region"  
b=b[,2:10] # MLS label is not included
b$askprice=b$askprice/10000   # scale some variables
b$ffarea=b$ffarea/100
b$mfee=b$mfee/10
b$sqfl=sqrt(b$floor)
print(table(b$region))
#    brentwood-park            cariboo    central-burnaby       central-park 
#                 9                  2                  1                  3 
#      east-burnaby            forglen    government-road           highgate 
#                 1                  6                  2                  5 
#         metrotown                sfu        south-slope   sullivan-heights 
#                23                  4                  6                  4 
# vancouver-heights willingdon-heights 
#                 2                  1 

n=nrow(b)
# categorical variable with 3 regions, 
# baseline group by default is first in alphabetical order
subreg=rep("other",n)
subreg
ireg1=(b$region=="metrotown"); ireg2=(b$region=="brentwood-park")

subreg[ireg1]="metrotown"
subreg[ireg2]="brentwood"
b$subreg=subreg

#the output for factor(subreg) is
# [1] metrotown brentwood other     metrotown metrotown metrotown metrotown metrotown other    
# [10] metrotown other     metrotown metrotown other     other     other     other     metrotown
# [19] other     other     other     other     other     other     brentwood brentwood other    
# [28] other     metrotown other     metrotown other     other     other     other     other    
# [37] other     metrotown other     metrotown metrotown other     other     other     metrotown
# [46] metrotown other     other     metrotown other     other     metrotown other     brentwood
# [55] metrotown other     metrotown metrotown brentwood other     brentwood brentwood other    
# [64] brentwood other     other     brentwood metrotown other    
# Levels: brentwood metrotown other
# to make metrotown the baseline,choose base = 2
subregm=C(factor(subreg),contr.treatment(3,base=2))
b$subregm=subregm

attach(b)   # attach data frame so that variables can be access without $
options(digits=6)
bur4=lm(askprice~ ffarea+sqfl+age+beds)
print(summary(bur4))
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   8.7044     2.9656    2.94   0.0046 ** 
#ffarea        2.5299     0.5918    4.27  6.5e-05 ***
#sqfl          2.5547     0.3685    6.93  2.4e-09 ***
#age          -0.5356     0.0557   -9.62  4.8e-14 ***
#beds          4.7587     1.9043    2.50   0.0150 *  
#Residual standard error: 4.85 on 64 degrees of freedom
#Multiple R-squared:  0.816,     Adjusted R-squared:  0.804

bur5=lm(askprice~ ffarea+sqfl+age+beds+subreg)
print(summary(bur5))
#Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       6.5353     3.2291    2.02    0.047 *  
#ffarea            2.7438     0.6019    4.56  2.5e-05 ***
#sqfl              2.3225     0.3784    6.14  6.5e-08 ***
#age              -0.5319     0.0560   -9.51  1.0e-13 ***
#beds              4.8213     1.9003    2.54    0.014 *  
#subregmetrotown   2.8090     1.9446    1.44    0.154    
#subregother      -0.0305     1.9089   -0.02    0.987    
#Residual standard error: 4.75 on 62 degrees of freedom
#Multiple R-squared:  0.828,     Adjusted R-squared:  0.812 

# baseline group above is "brentwood"

# Note that intercept has changed a lot but not the previous 4 betas.
# Interpretation: higher asking price in metrotown but not stat. significant
#    relative to baseline subregion of "brentwood". 

bur5m=lm(askprice~ ffarea+sqfl+age+beds+subregm)
print(summary(bur5m))
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    9.344      2.960    3.16   0.0025 ** 
#ffarea         2.744      0.602    4.56  2.5e-05 ***
#sqfl           2.322      0.378    6.14  6.5e-08 ***
#age           -0.532      0.056   -9.51  1.0e-13 ***
#beds           4.821      1.900    2.54   0.0137 *  
#subregm1      -2.809      1.945   -1.44   0.1536    
#subregm3      -2.840      1.415   -2.01   0.0491 *  
#Residual standard error: 4.75 on 62 degrees of freedom
#Multiple R-squared:  0.828,     Adjusted R-squared:  0.812 

# baseline group above is "metrotown"
# Interpretation: lower asking price in "other" relative to new baseline,
# it is marginally stat. significant

detach(b)
b$ibrent=as.numeric(subreg=="brentwood")
b$iother=as.numeric(subreg=="other")
bur5n=lm(askprice~ ffarea+sqfl+age+beds+ibrent+iother,data=b)
print(summary(bur5n))
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    9.344      2.960    3.16   0.0025 ** 
#ffarea         2.744      0.602    4.56  2.5e-05 ***
#sqfl           2.322      0.378    6.14  6.5e-08 ***
#age           -0.532      0.056   -9.51  1.0e-13 ***
#beds           4.821      1.900    2.54   0.0137 *  
#ibrent        -2.809      1.945   -1.44   0.1536    
#iother        -2.840      1.415   -2.01   0.0491 *  
#Residual standard error: 4.75 on 62 degrees of freedom
#Multiple R-squared:  0.828,     Adjusted R-squared:  0.812 

