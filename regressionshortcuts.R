prop <- function(a,b) {
  print(b/(a+b))
  return(a/(a+b))
}
lru <- function(data,outcomeindex){
  z <- setdiff(1:ncol(data),outcomeindex)
  for (i in z){
    if (length(levels(as.factor(data[,i]))) < 5) 
    {
      data[,i] <- as.factor(data[,i]);
      print("factor")
    }
    reg <- glm(data[,outcomeindex]~data[,i],family=binomial);
    cat(colnames(data)[i],coef(summary(reg))[-1,4],"\n")
    print(confint(reg))
    cat("\n")
  }
  cat("\n");
  print("Interactions");
  l <- combn(z,2);
  for (i in 1:ncol(l)) {
      a <- glm(data[,outcomeindex]~data[,l[1,i]]+data[,l[2,i]]+data[,l[1,i]]:data[,l[2,i]],family=binomial);
      cat(colnames(data)[l[1,i]],colnames(data)[l[2,i]]);
      print(coef(summary(a)))
      print(confint(a))
      cat("\n")
  }
}