
## ------------------------------------------------------------------
## ---- load packages
## ------------------------------------------------------------------
package.manager <- function(pkgs){
  loaded <- sapply(pkgs, function(pkg){
    if(require(pkg, character.only = TRUE)){
      TRUE
    }else
    {
      install.packages(pkg)
      require(pkg, character.only = TRUE)
    }
  })
  
  print('Package Util Require Result:')
  print(data.frame(Loaded = loaded, row.names = pkgs))
  
  invisible(all(loaded))
}


## ------------------------------------------------------------------
## --- do data ingest work
## ------------------------------------------------------------------
dataingest <- function(data){

  package.manager(c('dplyr', 'jsonlite'))

  # ---- the class of each type
  type <- sapply(data, class)

  ## ---- create summary of numeric data
  sum.number <- function(x, breaks = 31){
    
    h <- hist(x, plot = F, breaks = breaks)
    h.l <- list(x = h$breaks, y = h$counts)
    
    t.df <- data.frame(x = as.matrix(summary(x)))
    
    l <- list(h.l, t.df)
    
    toJSON(l)
    
  }
  
  sum.factor <- function(x, breaks = 31){
    
    
  }
  
  
  
  
  
  
  
  
  
}