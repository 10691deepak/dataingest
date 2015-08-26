
## ------------------------------------------------------------------
## ---- load required packages
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
package.manager(c('dplyr', 'jsonlite'))


## ------------------------------------------------------------------
## ---- do data summary work
## ------------------------------------------------------------------
data.summary <- function(data){

  numeric.summary <- function(x, type = 'numeric'){
    bars <- hist(x, plot = F)
    sum <- as.matrix(summary(x))
    result <- list(
      type = type,
      plot = list(x = bars$breaks, y = bars$counts),
      summary = list(labels = rownames(sum), values = as.numeric(sum)),
      missing = list(total = length(x), na = sum(is.na(x)))
    )
  }
  
  integer.summary <- function(x){
    numeric.summary(x, 'integer')
  }
  
  date.summary <- function(x){
    ## do parse thing
    numeric.summary(x, 'Date')
  }
  
  factor.summary <- function(x, type = 'factor'){
    sum <- as.matrix(summary(x))
    result <- list(
      type = 'factor',
      plot = list(x = rownames(sum), y = as.numeric(sum)),
      summary = list(labels = rownames(sum), values = as.numeric(sum)),
      missing = list(total = length(x), na = sum(is.na(x)))
    )
  }
  
  logical.summary <- function(x){
    factor.summary(x, 'logical')
  }
  
  ingest <- function(x){
      switch(class(x),
        'numeric' = numeric.summary(x),
        'integer' = integer.summary(x),
        'date'    = date.summary(x),
        'factor'  = factor.summary(x),
        'logical' = logical.summary(x)
      )
  }
  
  toJSON(lapply(data, ingest))

}