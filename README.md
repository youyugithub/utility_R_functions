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



```

cppFunction('
LogicalVector flag_smallest_n_cpp(
    NumericVector x,
    int n,
    LogicalVector filter,
    bool allow_tie,
    NumericVector if_tie){
  
  int length=x.length();
  NumericVector x_sort=x[filter];
  x_sort=na_omit(x_sort).sort();
  double x_thres;
  LogicalVector result(length);
  
  if(x_sort.length()==0){
    result.fill(false);
    return(result);
  }else if(x_sort.length()<n){
    x_thres=x_sort(x_sort.length()-1);
  }else{
    x_thres=x_sort(n-1);
  }
  
  int count=0;
  int i;
  for(i=0;i<length;i++){
    if(x(i)<=x_thres&&filter(i)){
      result(i)=true;
      count++;
    }else{
      result(i)=false;
    }
  }
  
  if(count<=n)return(result);
  if(allow_tie)return(result);
  
  NumericVector if_tie_sort=if_tie[result];
  if_tie_sort=na_omit(if_tie_sort).sort();
  double if_tie_thres;
  LogicalVector result0(length);
  
  if(if_tie_sort.length()==0){
    if_tie.fill(0.0);
    if_tie_thres=0.0;
  }else if(if_tie_sort.length()<n){
    if_tie_thres=if_tie_sort(if_tie_sort.length()-1);
  }else{
    if_tie_thres=if_tie_sort(n-1);
  }
  
  count=0;
  for(i=0;i<length;i++){
    if(result(i)&&if_tie(i)<=if_tie_thres&&count<n){
      result0(i)=true;
      count++;
    }else{
      result0(i)=false;
    }
  }
  return(result0);
}')

flag_smallest_n(
  c(NA,NA,NA,0,1,2,1,1,2),n=2,allow_tie=T)
```
find the first true value
```
cppFunction('
double value_at_true(NumericVector x,LogicalVector filter){
  int n=x.length();
  int i=0;
  for(i=0;i<n;i++){
    if(filter[i]){
      return(x[i]);
    }
  }
  return(NA_REAL);
}
')
```
example
```
df<-data.frame(
  id=c(1,1,1,1,1,2,2,2,2,2),
  value=as.Date(c(1:5,1:5)),
  filter=c(F,F,T,T,F,F,F,F,F,F))
df%>%
  group_by(id)%>%
  mutate(x=as.Date(value_at_true(value,filter)))
```


```
cppFunction('
LogicalVector whether_percent_increase_within_window(
    NumericVector x, 
    NumericVector day,
    double percent,
    double window) {
  
  int n=x.length();
  LogicalVector y(n,false);
  int i=0,j=0;
  
  double percent1=percent+1.0;
  for(i=0;i<n;i++){
    if(NumericVector::is_na(x[i])){
      y[i]=NA_LOGICAL;
      continue;
    }
    for(j=i-1;j>=0;j--){
      if(NumericVector::is_na(x[j]))continue;
      if(std::abs(day[i]-day[j])>window)continue;
      if(x[i]>=percent1*x[j]){
        y[i]=true;
        break;
      }
    }
  }
  return(y);
}')
```
```
cppFunction('
NumericVector na_locf_num(
    NumericVector x,
    double x_default=NA_REAL) {
  
  int n=x.length();
  int i=0;
  double current=x_default;
  for(i=0;i<n;i++){
    if(NumericVector::is_na(x[i])){
      x[i]=current;
    }else{
      current=x[i];
    }
  }
  return(x);
}
')
```

```
cppFunction('
IntegerVector n_consecutively_confirmed(
    LogicalVector x) {
  
  int n=x.length();
  int i=0;
  int count=0;
  IntegerVector y(n,NA_REAL);
  for(i=0;i<n;i++){
    if(LogicalVector::is_na(x[i])){
      continue;
    }else if(x[i]){
      count++;
      y[i]=count;
    }else if(!x[i]){
      count=0;
      y[i]=count;
    }
  }
  return(y);
}
')
```

```
// [[Rcpp::export]]
IntegerVector confirmed_multiple_aab( // are the last two non_na positive?
    IntegerMatrix x) {
  
  int n=x.nrow();
  int k=x.ncol();
  int i,j;
  IntegerVector prev1(k,NA_INTEGER);
  IntegerVector prev2(k,NA_INTEGER);
  int count_prev1,count_prev2;
  IntegerVector multiple(n);
  
  for(i=0;i<n;i++){
    for(j=0;j<k;j++){
      if(!IntegerVector::is_na(x(i,j))){
        prev2(j)=prev1(j);
        prev1(j)=x(i,j);
      }
    }
    count_prev1=0;
    count_prev2=0;
    for(j=0;j<k;j++){
      if(prev1(j)>=1)count_prev1++;
      if(prev2(j)>=1)count_prev2++;
    }
    if(count_prev1>=2&&count_prev2>=2){
      multiple(i)=1;
    }else{
      multiple(i)=0;
    }
  }
  return(multiple);
}

// [[Rcpp::export]]
IntegerVector naab(
    IntegerMatrix x) {
  
  int n=x.nrow();
  int k=x.ncol();
  int i,j;
  IntegerVector prev1(k,NA_INTEGER);
  int count_prev1;
  IntegerVector multiple(n);
  
  for(i=0;i<n;i++){
    for(j=0;j<k;j++){
      if(!IntegerVector::is_na(x(i,j))){
        prev1(j)=x(i,j);
      }
    }
    count_prev1=0;
    for(j=0;j<k;j++)if(prev1(j)>=1)count_prev1++;
    multiple(i)=count_prev1;
  }
  return(multiple);
}
```
