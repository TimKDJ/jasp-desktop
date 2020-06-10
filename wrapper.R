bancova <- function(formula           = NULL,
                    data              = NULL,
                    nullModel         = NULL,
                    dep               = "",
                    fixed             = c(),
                    random            = c(),
                    covariates        = c(),
                    model             = c(),
                    priorFixed        = 0.5,
                    priorRandom       = 1,
                    priorCovariates   = 0.354,
                    effects           = FALSE,
                    effectsType       = "allModels",
                    postEst           = FALSE,
                    ci                = 0.95,
                    criTable          = FALSE,
                    bfOrder           = "bestModelTop",
                    bfType            = "BF10",
                    groupPost         = "grouped",
                    singleModel       = c(),
                    singleEst         = FALSE,
                    singleCriTable    = FALSE,
                    singlePostPlot    = FALSE,
                    singleQqPlot      = FALSE,
                    singleRsqPlot     = FALSE,
                    singleGroupPost   = "grouped",
                    postHocVars       = c(),
                    postHocNull       = TRUE,
                    descr             = FALSE,
                    varOnYAxis        = "",
                    varAsLines        = "",
                    varAsPlots        = "",
                    wantPlotCi        = FALSE,
                    plotCi            = 0.95,
                    postPlot          = FALSE,
                    qqPlot            = FALSE,
                    rsqPlot           = FALSE,
                    fixedNumAcc       = 1,
                    fixedMCMCSamples  = 1,
                    sampleModeNumAcc  = "auto",
                    sampleModeMCMC    = "auto") {
   
   defaultArgCalls <- formals(as.character(match.call()[[1]]))
   defaultArgs <- lapply(defaultArgCalls, eval)
   
   options <- as.list(match.call())
   options <- modifyList(options, defaultArgs)
   options[[1]] <- NULL
   options[["data"]] <- NULL

   if (!missing(dep))
      dep <- eval(substitute(eval(dep)), setNames(as.list(names(data)), names(data)))
   if (!missing(fixed))
      fixed <- eval(substitute(eval(fixed)), setNames(as.list(names(data)), names(data)))
   if (!missing(random))
      random <- eval(substitute(eval(random)), setNames(as.list(names(data)), names(data)))
   if (!missing(covariates))
      covariates <- eval(substitute(eval(covariates)), setNames(as.list(names(data)), names(data)))
   if (!missing(postHocVars))
      postHocVars <- eval(substitute(eval(postHocVars)), setNames(as.list(names(data)), names(data)))
   if (!missing(varOnYAxis))
      varOnYAxis <- eval(substitute(eval(varOnYAxis)), setNames(as.list(names(data)), names(data)))
   if (!missing(varAsLines))
      varAsLines <- eval(substitute(eval(varAsLines)), setNames(as.list(names(data)), names(data)))
   if (!missing(varAsPlots))
      varAsPlots <- eval(substitute(eval(varAsPlots)), setNames(as.list(names(data)), names(data)))
browser()
   if (!is.null(formula)) {
      if (!inherits(formula, "formula"))
         formula <- as.formula(formula)
      
      if (dep != "") 
         formula  <- addToFormula(formula, terms = dep, type = "lhs")
      
      if (length(fixed) > 0)
         formula <- addToFormula(formula, terms = fixed, type = "rhs")
      
      if (length(random) > 0)
         formula <- addToFormula(formula, terms = random, type = "rhs")
      
      if (length(covariates) > 0)
         formula <- addToFormula(formula, terms = covariates, type = "rhs")
      
      if (length(model) == 0)
         options$model <- createModelFromFormula(formula, type = "rhs", nuisance = list(name = "isNuisance", terms = nullModel))
      
      options$dep <- getFromFormula(formula, data, type = "lhs", mode = "numeric", exclude = random, nullValue = defaultArgs[["dep"]])
      options$fixed <- getFromFormula(formula, data, type = "rhs", mode = "factor", exclude = random, nullValue = defaultArgs[["fixed"]])
      options$covariates <- getFromFormula(formula, data, type = "rhs", mode = "numeric", exclude = random, nullValue = defaultArgs[["covariates"]])
   }

   if (length(singleModel) == 0)
      options$singleModel <- createModelFromFormula(formula, type = "rhs")
   
   # this will go as name changes are permanent
   options <- convertOptionNames(options)
   
   if (!require(jasptools)) stop("for now we still need jasptools to run this :)")
   jasptools::run("AncovaBayesian", dataset=data, options=options)
   
   #jaspResults <- initJaspResults()
   #JASP:::AncovaBayesian(jaspResults=jaspResults, dataset=data, options=options)
   #summary(jaspResults)
   #return(invisible(jaspResults$asResults()))
}

dataVarsToStrings <- function(vars, data) {
   vars <- substitute(eval(vars))
   eval(vars, setNames(as.list(names(data)), names(data)))
}

createModelFromFormula <- function(formula, type, nuisance = list(), excludeInteractions = FALSE) {
   if (! type %in% c("lhs", "rhs"))
      stop("Can only get rhs or lhs terms from a formula")
   
   components <- splitFormula(formula, excludeInteractions)[[type]]
   
   model <- vector("list", length(components))
   for (i in seq_along(components)) {
      model[[i]] <- createModelComponentItem(components[[i]], nuisance)
   }
   
   return(model)
}

createModelComponentItem <- function(component, nuisance) { # nuisance needs to be expanded to other column types..
   result <- list()
   result[["components"]] <- expandInteractionTerm(component)
   if (length(nuisance) > 0) {
      
      nuisanceTerms <- NULL
      if (inherits(nuisance[["terms"]], "formula"))
         nuisanceTerms <- splitFormula(nuisance[["terms"]])[["rhs"]]
      
      result[[nuisance[["name"]]]] <- isTRUE(component %in% nuisanceTerms)
   }
   
   return(result)
}

expandInteractionTerm <- function(term) {
   return(unlist(strsplit(term, ":", fixed = TRUE)))
}

getFromFormula <- function(formula, data, type, mode = NULL, exclude = c(), nullValue = NULL) {
   if (! type %in% c("lhs", "rhs"))
      stop("Can only get rhs or lhs terms from a formula")
   
   if (!is.data.frame(data))
      stop("Expecting data to be a data.frame")
   
   components <- splitFormula(formula, excludeInteractions = TRUE)[[type]]
   components <- getComponentsOfCorrectMode(components, data, mode)
   components <- setdiff(components, exclude)
   
   if (length(components) == 0)
      components <- nullValue
   
   return(components)
}

getComponentsOfCorrectMode <- function(components, data, mode) {
   if (!all(components %in% names(data)))
      stop("Not all formula components appear in the dataset")
   
   result <- NULL
   for (component in components) {
      if (mode == "factor" && is.factor(data[[component]]))
         result <- c(result, component)
      else if (mode == "numeric" && is.numeric(data[[component]]))
         result <- c(result, component)
   }
   
   return(result)
}

splitFormula <- function(formula, excludeInteractions = FALSE) {
   if (!inherits(formula, "formula"))
      stop("Expecting formula to be of class `formula`")
   
   terms <- as.character(formula)
   if (length(terms) < 3) { # there is no lhs
      lhs <- NULL
      rhs <- terms[2]
   } else {
      lhs <- terms[2]
      rhs <- terms[3]
   }
   
   return(list(lhs = getComponentsFromTerm(lhs, excludeInteractions),
               rhs = getComponentsFromTerm(rhs, excludeInteractions)))
}

addToFormula <- function(formula, terms, type) {
   ### what about interactions?
   if (! type %in% c("lhs", "rhs"))
      stop("Can only add rhs or lhs terms to a formula")
   
   if (!is.character(terms))
      stop("Expecting character vector of terms")
   
   components <- splitFormula(formula)
   
   if (type == "lhs")
      components[["lhs"]] <- union(components[["lhs"]], terms)
   else
      components[["rhs"]] <- union(components[["rhs"]], terms)
   
   return(makeFormula(lhs = components[["lhs"]], rhs = components[["rhs"]]))
}

getComponentsFromTerm <- function(term, excludeInteractions) {
   if (!is.character(term) || nchar(term) == 0)
      return(NULL)
   
   components <- trimws(unlist(strsplit(term, "+", fixed = TRUE)))
   
   if (excludeInteractions) {
      purgedComponents <- NULL
      for (component in components)
         if (!grepl(":", component, fixed = TRUE))
            purgedComponents <- c(purgedComponents, component)
         components <- purgedComponents
   }
   
   return(components)
}

makeFormula <- function(lhs, rhs, env = parent.frame(2)) {
   lhs <- paste(lhs, collapse = "+")
   rhs <- paste(rhs, collapse = "+")
   
   return(formula(paste(lhs, "~", rhs), env = env))
}

convertOptionNames <- function(options) {
   mapping <- list(dep               = "dependent", # formula, nullModel
                   fixed             = "fixedFactors",
                   random            = "randomFactors",
                   covariates        = "covariates",
                   model             = "modelTerms",
                   priorFixed        = "priorFixedEffects",
                   priorRandom       = "priorRandomEffects",
                   priorCovariates   = "priorCovariates",
                   effects           = "effects",
                   effectsType       = "effectsType",
                   postEst           = "posteriorEstimates",
                   ci                = "credibleInterval",
                   criTable          = "criTable",
                   bfOrder           = "bayesFactorOrder",
                   bfType            = "bayesFactorType",
                   groupPost         = "groupPosterior",
                   singleModel       = "singleModelTerms",
                   singleEst         = "singleModelEstimates",
                   singleCriTable    = "singleModelCriTable",
                   singlePostPlot    = "singleModelPosteriorPlot",
                   singleQqPlot      = "singleModelqqPlot",
                   singleRsqPlot     = "singleModelrsqPlot",
                   singleGroupPost   = "singleModelGroupPosterior",
                   postHocVars       = "postHocTestsVariables",
                   postHocNull       = "postHocTestsNullControl",
                   descr             = "descriptives",
                   varOnYAxis        = "plotHorizontalAxis",
                   varAsLines        = "plotSeparateLines",
                   varAsPlots        = "plotSeparatePlots",
                   wantPlotCi        = "plotCredibleInterval",
                   plotCi            = "plotCredibleIntervalInterval",
                   postPlot          = "posteriorPlot",
                   qqPlot            = "qqPlot",
                   rsqPlot           = "rsqPlot",
                   fixedNumAcc       = "fixedNumAcc",
                   fixedMCMCSamples  = "fixedMCMCSamples",
                   sampleModeNumAcc  = "sampleModeNumAcc",
                   sampleModeMCMC    = "sampleModeMCMC")
   
   oldNames <- names(options)
   newNames <- oldNames
   for (i in seq_along(oldNames))
      if (oldNames[i] %in% names(mapping))
         newNames[i] <- mapping[[oldNames[i]]]
   
   names(options) <- newNames
   
   return(options)
}