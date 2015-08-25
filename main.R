
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

package.manager(c('dplyr', 'jsonlite'))

## ------------------------------------------------------------------
## --- do data ingest work
## ------------------------------------------------------------------
dataingest <- function(data){

  ## ---------------------------------------------------------------- 
  ## ---- data ingest functions
  ## ----------------------------------------------------------------
  
  ## ---- produces json object
  ## { histogram: [breaks [ .. ], counts [ ..] ], 
  ##   summary: [labels [ .. ], values[ .. ]] }
  numeric.summary <- function(x, type = 'numeric'){
    
    breaks <- 31
    
    bars <- hist(x, plot = F, breaks = breaks)
    sum <- as.matrix(summary(x))
    
    result <- list(
      type = type,
      histogram = list(breaks = bars$breaks, counts = bars$counts),
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
  
  ## ---------------------------------------------------------------- 
  ## ---- do work
  ## ----------------------------------------------------------------
  
  toJSON(lapply(data, ingest))

}