deft_colors <- c(
  `Never deforested`="#136b3a",
  `Never deforested, converted to pulp plantation`="#7fad9a",
  `Never deforested, not converted to pulp plantation` = "#136b3a",
  `Deforestation >3 years before license`="#733f3c",
  `Deforestation >3 years before license, converted to pulp plantation`="#733f3c",
  `Deforestation >3 years before license, not converted to pulp plantation`="#EB4A40",
  `Deforestation in 3 years prior to license`="#cc8037",
  `Deforestation in 3 years prior to license, converted to pulp plantation`="#cc8037",
  `Deforestation in 3 years prior to license, not converted to pulp plantation`="#F3AD6A",
  `Deforestation on licensed concession, before earliest ZDC of downstream mill`="#4b0e82",
  `Deforestation on licensed concession, before earliest ZDC of downstream mill, converted to pulp plantation`="#4b0e82",
  `Deforestation on licensed concession, before earliest ZDC of downstream mill, not converted to pulp plantation`="#e30ece",
  `Deforestation on licensed concession, after earliest ZDC of downstream mill`="#b681e6",
  `Deforestation on licensed concession, after earliest ZDC of downstream mill, converted to pulp plantation`="#b681e6",
  `Deforestation on licensed concession, after earliest ZDC of downstream mill, not converted to pulp plantation`="#f299f1",
  `Deforestation on concession after license`= "#e6e08e",
  `Deforestation on concession after license, converted to pulp plantation` ="#e6e08e",
  `Deforestation on concession after license, not converted to pulp plantation`="#d1e820")

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

cols = c(deft_cols("Never deforested"),
         deft_cols("Never deforested, converted to pulp plantation"),
         deft_cols("Never deforested, not converted to pulp plantation"),
         deft_cols("Deforestation >3 years before license"),
         deft_cols("Deforestation >3 years before license, converted to pulp plantation"),
         deft_cols("Deforestation >3 years before license, not converted to pulp plantation"),
         deft_cols("Deforestation in 3 years prior to license"),
         deft_cols("Deforestation in 3 years prior to license, converted to pulp plantation"),
         deft_cols("Deforestation in 3 years prior to license, not converted to pulp plantation"),
         deft_cols("Deforestation on licensed concession, before earliest ZDC of downstream mill"),
         deft_cols("Deforestation on licensed concession, before earliest ZDC of downstream mill, converted to pulp plantation"),
         deft_cols("Deforestation on licensed concession, before earliest ZDC of downstream mill, not converted to pulp plantation"),
         deft_cols("Deforestation on licensed concession, after earliest ZDC of downstream mill"),
         deft_cols("Deforestation on licensed concession, after earliest ZDC of downstream mill, converted to pulp plantation"),
         deft_cols("Deforestation on licensed concession, after earliest ZDC of downstream mill, not converted to pulp plantation"),
         deft_cols("Deforestation on concession after license"),
         deft_cols("Deforestation on concession after license, converted to pulp plantation"),
         deft_cols("Deforestation on concession after license, not converted to pulp plantation")
)

cols_alpha <- add.alpha(cols, alpha=0.75)

