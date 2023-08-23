deft_colors <- c(
  `Never deforested`="#136b3a",
  `Deforestation pre-permit`="#EB4A40",
  `Deforestation post-permit`="#FFBF00",
  `Deforestation post-permit and after first ZDC of downstream mill`="#4b0e82"
  #`Never deforested, not converted to pulp plantation` = "#136b3a",
  #`Deforestation pre-permit and first planting`="#EB4A40",
  #`Deforestation pre-permit and first planting, converted to pulp plantation`="#EB4A40",
  #`Deforestation pre-permit and first planting, not converted to pulp plantation`="#ed8f8a",
  #`Deforestation post-permit / post-first planting`="#94764d",
  #`Deforestation post-permit / post-first planting, converted to pulp plantation`="#94764d",
  #`Deforestation post-permit / post-first planting, not converted to pulp plantation`="#dfc398",
  #`Deforestation post-permit / post-first planting and after first ZDC of downstream mill`="#4b0e82",
  #`Deforestation post-permit / post-first planting and after first ZDC of downstream mill, converted to pulp plantation`="#7827c2",
  #`Deforestation post-permit / post-first planting and after first ZDC of downstream mill, not converted to pulp plantation`="#c194d4")
  #`Deforestation on licensed concession, after earliest ZDC of downstream mill`="#b681e6",
  #`Deforestation on licensed concession, after earliest ZDC of downstream mill, converted to pulp plantation`="#b681e6",
  #`Deforestation on licensed concession, after earliest ZDC of downstream mill, not converted to pulp plantation`="#f299f1",
  #`Deforestation on concession after license`= "#e6e08e",
  #`Deforestation on concession after license, converted to pulp plantation` ="#e6e08e",
  #`Deforestation on concession after license, not converted to pulp plantation`="#d1e820"
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
         deft_cols("Deforestation pre-permit"),
         deft_cols("Deforestation post-permit"),
         deft_cols("Deforestation post-permit and after first ZDC of downstream mill")
)

cols_alpha <- add.alpha(cols, alpha=0.70)

