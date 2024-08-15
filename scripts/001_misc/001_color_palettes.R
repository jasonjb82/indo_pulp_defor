deft_colors <- c(
  `Remaining forest`="#009E73",
  `Deforestation for pulp during 2001-2015`="#F0E442",
  `Deforestation not for pulp`="#0072B2",
  `Deforestation for pulp after 2015`="#CC79A7"
)

deft_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (deft_colors)
  
  deft_colors[cols]
}

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

cols = c(deft_cols("Remaining forest"),
         deft_cols("Deforestation for pulp during 2001-2015"),
         deft_cols("Deforestation not for pulp"),
         deft_cols("Deforestation for pulp after 2015")
)

cols_alpha <- add.alpha(cols, alpha=0.85)

