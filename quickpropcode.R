prop.sum <- function(var,name,levels) {
    t <- table(var)
     print(t)
     k <- prop.table(t)
     print(k)
     cat(name,"& \\makecell[l]{")
     for (i in 1:length(levels)) {
       cat(levels[i],":",round(t[i],digits=2),"(",round(k[i]*100,digits=2),"\\%",")","\\\\",sep="")
     }
     if (length(which(is.na(var))) == 0) { 
    cat("} \\\\","\\hline")
     }
     else {
       cat("Missing:",length(which(is.na(var))), "} \\\\","\\hline")
     }
}

prop.sum2 <- function(var,levels) {
  t <- table(var)
  print(t)
  k <- prop.table(t)
  print(k)
  for (i in 1:length(levels)) {
    cat(levels[i],":",round(t[i],digits=2),"(",round(k[i]*100,digits=2),"%",")","\n",sep="")
  }
  if (length(which(is.na(var))) == 0) { 
   return(cat("\n"))
  }
  else {
    cat("Missing:",length(which(is.na(var))))
  }
}