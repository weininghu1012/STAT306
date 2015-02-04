#lab4 for loop
N <- 63445
sum <- 0
for (i in 1:(N-1)){
  if ((i%%89 ==0)|(i%%107 == 0)){
    sum <- sum + sqrt(i);
  }
}
print(sum)

