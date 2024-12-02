# utility_R_functions

```
cb <- function(df, sep="\t", dec=".", max.size=(200*1000)){
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

# which function is called

```
fun_seeker <- function(obj,fun) {
  funs_to_check <- paste(fun,c(class(obj),"default"),sep=".")
  funs_exists <- funs_to_check %in% methods(fun)
  if (any(funs_exists)) funs_to_check[funs_exists][1] else stop("No applicable method")
}
fun_seeker(matrix(rnorm(100),10),"plot")
fun_seeker(matrix(rnorm(100),10),"summary")

fun_seeker(fit_tree,"plot")
```

# Rcpp for dplyr

```
cppFunction('
IntegerVector count_hits(LogicalVector x){
  int n=x.length();
  int count=0;
  IntegerVector y(n,NA_INTEGER);
  for(int i=0;i<n;i++){
    if(LogicalVector::is_na(x[i]))continue;
    if(!x[i])continue;
    count++;
    y[i]=count;
  }
  return(y);
}')

cppFunction('
NumericVector cumsum_skip_na(NumericVector x){
  int n=x.length();
  double sum=0.0;
  NumericVector y(n,NA_REAL);
  for(int i=0;i<n;i++){
    if(NumericVector::is_na(x[i]))continue;
    sum+=x[i];
    y[i]=sum;
  }
  return(y);
}')


cppFunction('
NumericVector lag_not_na_num(NumericVector x){
  int n=x.length();
  NumericVector y(n,NA_REAL);
  int i=0,j=0;
  for(i=0;i<n;i++){
    for(j=i-1;j>=0;j--){
      if(!NumericVector::is_na(x[j])){
        y[i]=x[j];
        break;
      }
    }
  }
  return(y);
}')

cppFunction('
double first_num(NumericVector x,LogicalVector filter,int n=1){
  int xlength=x.length();
  double y=R_NaReal;
  int i=0,count=0;
  for(i=0;i<xlength;i++){
    if(filter(i)){
      count++;
      if(count==n)y=x(i);
    }
  }
  return(y);
}')

cppFunction('
NumericVector lag_num(NumericVector x,LogicalVector filter,bool na_rm=false){
  int n=x.length();
  NumericVector y(n,NA_REAL);
  int i=0,j=0;
  if(na_rm){
    for(i=0;i<n;i++){
      for(j=i-1;j>=0;j--){
        if(LogicalVector::is_na(x[j]))continue;
        if(filter[j]){
          y[i]=x[j];
          break;
        }
      }
    }
  }else{
    for(i=0;i<n;i++){
      for(j=i-1;j>=0;j--){
        if(filter[j]){
          y[i]=x[j];
          break;
        }
      }
    }
  }
  return(y);
}')


cppFunction('
NumericVector lag_narm_num(NumericVector x){
  int n=x.length();
  NumericVector y(n,NA_REAL);
  int i=0,j=0;
  for(i=0;i<n;i++){
    for(j=i-1;j>=0;j--){
      if(!NumericVector::is_na(x[j])){
        y[i]=x[j];
        break;
      }
    }
  }
  return(y);
}')


```
