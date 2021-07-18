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
