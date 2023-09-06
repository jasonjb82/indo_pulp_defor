
deft_colors <- c(
  `Never deforested`="#009E73",
  `Deforestation for pulp before first ZDC of downstream mill`="#F0E442",
  `Deforestation not for pulp`="#0072B2",
  `Deforestation for pulp after first ZDC of downstream mill`="#CC79A7"
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

# cols = c(deft_cols("Never deforested"),
#          deft_cols("Never deforested, not converted to pulp plantation"),
#          deft_cols("Deforestation pre-permit and first planting"),
#          deft_cols("Deforestation pre-permit and first planting, not converted to pulp plantation"),
#          deft_cols("Deforestation pre-permit and first planting, converted to pulp plantation"),
#          deft_cols("Deforestation post-permit / post-first planting"),
#          deft_cols("Deforestation post-permit / post-first planting, not converted to pulp plantation"),
#          deft_cols("Deforestation post-permit / post-first planting, converted to pulp plantation"),
#          deft_cols("Deforestation post-permit / post-first planting and after first ZDC of downstream mill"),
#          deft_cols("Deforestation post-permit / post-first planting and after first ZDC of downstream mill, not converted to pulp plantation"),
#          deft_cols("Deforestation post-permit / post-first planting and after first ZDC of downstream mill, converted to pulp plantation")
# 
# )

cols = c(deft_cols("Never deforested"),
         deft_cols("Deforestation for pulp before first ZDC of downstream mill"),
         deft_cols("Deforestation not for pulp"),
         deft_cols("Deforestation for pulp after first ZDC of downstream mill")
)

cols_alpha <- add.alpha(cols, alpha=0.85)

