# utility_R_functions

```
cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
    # Copy a data.frame to clipboard
    write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
  }
```
