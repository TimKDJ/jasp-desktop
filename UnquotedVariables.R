# possible types: single, vector, list of vectors (pairs), interaction, with row components
main <- function(single, vector, pairs, interaction, rowComponents, data) {
  single        <- toStringTerms(substitute(single),        data)
  vector        <- toStringTerms(substitute(vector),        data)
  pairs         <- toStringTerms(substitute(pairs),         data, type = "paired")
  interaction   <- toStringTerms(substitute(interaction),   data)
  rowComponents <- toStringTerms(substitute(rowComponents), data, type = "rowComponents")
  
  print(single); print(vector); print(pairs); print(interaction); print(rowComponents)
}

toStringTerms <- function(unevalExpr, data, type = "regular") {
  if (type == "rowComponents" || type == "paired") {
    lst <- as.list(unevalExpr)

    if (lst[[1]] != "list")
      stop("Argument requires a list of elements")
    
    lst[[1]] <- NULL
    
    if (type == "rowComponents")
      return(stringifyRowComponents(lst))
    
    if (type == "paired")
      return(stringifyPairs(lst))
  } else {
    return(stringifyTerms(unevalExpr, data))
  }
}

stringifyRowComponents <- function(lst) {
  if (!"terms" %in% names(lst))
    stop("Argument needs to be a named list with the entry `terms`")
  
  lst[["terms"]] <- stringifyTerms(lst[["terms"]], data)
  
  return(lst)
}

stringifyPairs <- function(lst) {
  lapply(lst, stringifyTerms, data = data)
}

stringifyTerms <- function(unevalExpr, data) {
  terms <- asCharacterVector(unevalExpr)
  
  for (term in terms) {
    interactions <- hasInteractions(terms)
    if (hasInteractions(term)) {
      
      components <- splitInteractions(terms)
      if (!all(termIsValidColumn(components, data)))
        stop("The interaction term (", term, ") contains unknown data columns")
      
    } else if (!termIsValidColumn(term, data)) {
        stop("The term (", term, ") could not be found in the data")
    }
  }
  
  return(terms)
}

asCharacterVector <- function(unevalExpr) {
  result <- unevalExpr

  charRepresent <- as.character(unevalExpr)
  if (inherits(unevalExpr, "name")) # e.g., contNormal
    result <- charRepresent
  else if (inherits(unevalExpr, "call") && charRepresent[[1]] == "c" && length(charRepresent) > 1) # e.g., c(contNormal, contGamma)
    result <- charRepresent[2:length(charRepresent)]
  else if (inherits(unevalExpr, "call")) # e.g., contNormal:contGamma
    result <- deparse(unevalExpr)

  return(result)
}

termIsValidColumn <- function(term, data) {
  term %in% names(data)
}

hasInteractions <- function(term) {
  grepl(":", term)
}

splitInteractions <- function(term) {
  unlist(stringr::str_split(term, ":"))
}


###### -------- example


data <- data.frame(col1 = 1:10, col2 = 1:10, col3 = 1:10)
main(single        = col1, 
     vector        = c("col1", col2), 
     pairs         = list(c(col1, col2), c(col1, col3)), 
     interaction   = c(col1, col2, col1:col2),
     rowComponents = list(terms = c(col1, "col1:col2", col2),
                          null  = c(T, T, F)),
     data          = data)