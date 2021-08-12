# utility_R_functions

```
cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
    # Copy a data.frame to clipboard
    write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
  }
```

# list to matrix

https://stackoverflow.com/questions/15201305/how-to-convert-a-list-consisting-of-vector-of-different-lengths-to-a-usable-data
```
word.list <- list(letters[1:4], letters[1:5], letters[1:2], letters[1:6])
n.obs <- sapply(word.list, length)
seq.max <- seq_len(max(n.obs))
mat <- t(sapply(word.list, "[", i = seq.max))
```

# loess outlier removal

```
wt.bisquare <- function(u, c=6) {
  ifelse( abs(u/c) < 1, (1-(u/c)^2)^2, 0)
}

xx<-sort(3*rnorm(100))
yy<-sin(xx)+rnorm(100,0,0.5)
yy[30]<-10

lo<-loess(yy~xx,span=1/3)

wt<-rep(1,length(xx))
for(i in 1:10){
  lo2<-loess(yy~xx,weights=wt,span=1/3)
  wt<-wt.bisquare(lo2$res/median(abs(lo2$res)),c=6)
}

# Plot the data
plot(yy~xx,pch=16)

# Add the robust loess
lines(xx,predict(lo2),col="red")
```

# lm outlier removal

```
calculate_fitted<-function(x,y){
  idx<-!(is.na(x)|is.na(y))
  if(sum(idx)>3){
    xsub<-x[idx]
    ysub<-y[idx]/100
    afit<-MASS::rlm(ysub~xsub,maxit=50,method="MM",psi=psi.bisquare)
    a<-coef(afit)[1]
    b<-coef(afit)[2]
    return((a+b*x)*100)
  }else if(sum(idx)==3){
    result<-rep(NA,length(x))
    result[idx]<-median(y,na.rm=TRUE)
    return(result)
  }else if(sum(idx)==2){
    result<-rep(NA,length(x))
    result[which(idx)[1]]<-y[which(idx)[2]]
    result[which(idx)[2]]<-y[which(idx)[1]]
    return(result)
  }else{
    return(y)
  }
}
```
