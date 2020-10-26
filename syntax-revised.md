# Part I: Definition and Goals

## Definition
Syntax mode refers to the ability for users to see what code generates the output they see. This includes filters, computed columns and analyses.

## Use Case
1. Syntax mode will provide a smoother transition from SPSS that makes heavy use of syntax.
2. With syntax mode it becomes easier to develop new R analyses as they can be run from RStudio with little effort.
3. Syntax can easily be shared between colleagues and attached to appendices.
4. JASP can be a tool to expose students to R.

## Primary Goals of Syntax Mode Implementation
1. The syntax will cover filters, computed columns and analyses.
2. The syntax will be valid R and can be run outside of JASP in RStudio and in the R editor of JASP.
3. The syntax will be consistent with the syntax you would provide to regular R packages (e.g., `stats::lm()`).
4. The user will need to perform minimal additional steps after copy-pasting to generate output similar of that seen in JASP (i.e., only read data).
5. The output of the syntax in RStudio will generate a structure that R users are familiar with.
6. The syntax is set up in a way that will, **in the future**, allow users to execute syntax inside of JASP and have JASP perform the specified filters/computed columns/analyses.
7. The user can easily browse through documentation of the syntax within RStudio.

## Intended Limitations of Syntax Mode
1. The syntax does not need to be executable in JASP at present (other than in the R editor), but we'll likely want that at a later stage (see also bullet 6 of the Primary Goals).
2. The syntax can be separate statements that are not linked together. What this means is that it is acceptable not to include package installing, data reading and temporary variables (although we could add this as an additional feature). By doing this we will ensure it will be easier to later execute our generated syntax inside of JASP (once we reach that point). An example of the distincting between linked and separate statements:
   
Separate statements:
```
filter(...)
mutate(...)
anova(...)
```
Linked statements:
```
if (!require(jasp)) install.packages("jasp")
library(jasp)
read.csv(...) %>% filter(...)
              %>% mutate(...)
              %>% anova(...)
```



# Part II: Examples of the Intended Syntax

## Filters
Based on `dplyr::filter()`, with only one proposed adjustment to dplyr's logic: allow no data to be passed in, in which case we search for a dataset in the `parent.frame`. This ensures we don't have to enter `dplyr::filter(data = dataset, ...)` everywhere and make it easier to execute inside the R editor of JASP (which has a fixed name). The following filters are already natively supported by `dplyr::filter()` and return filtered `data.frame`s:

From JASP's "drag 'n drop interface" filter:

![image info](Single-line-filter.png)
```
filter(contNormal < 0)
```
From JASP's "R interface" filter:

![image info](Multi-line-filter.png)
```
filter({
    temp1 <- facExperim
    temp1 == 1 & contBinom == 0
})
```

## Computed Columns
Based on `dplyr::mutate()`, with the same proposed change of removing `data` (and a rename of the function to `compute`). The following ways of column adding are already natively supported by `dplyr::mutate()` and return `data.frame`s with the additional columns:

From JASP's "drag 'n drop interface" compute column:

![image info](Single-line-compute-column-naming.png)

![image info](Single-line-compute-column.png)
```
compute(sumScore = contNormal + contGamma)
```

From JASP's "R interface" compute column:

![image info](Multi-line-compute-column-naming.png)

![image info](Multi-line-compute-column.png)
```
compute(logSumScore = {
  sumScore <- contNormal + contGamma
  log(sumScore)
})
```

## Analyses
These functions are intended to be alike to statistical functions such as `stats::lm()`, especially in regards to the use of formula's. JASP's analysis functions should only deviate from other R functions when it comes to output. Whereas JASP will creates lots of output from a single function call, regular R functions generally only do "one thing". However, our approach isn't necessarily worse than the norm as it requires the users to only know about one single function. To accomplish a clean interface for the analyses, we will need wrappers around the existing analyses. This is an example call to a wrapper around our ANOVA analysis:

![image info](ANOVA-analysis-call.png)

```
anova(formula = contNormal ~ facGender + facExperim + facGender * facExperim,
      descriptives = TRUE)
```

#### Formula's
Formula's complicate the implementation, but simplify the syntax. Furthermore, they are the standard way in which models are specified in R. The next example shows the benefit of formula's by repeating the previous ANOVA call, but without using a formula:
```
anova(dep = "contNormal",
      fixed = c("facGender", "facExperim"),
      modelTerms = list(list(components = "facGender"), list(components = "facExperim"), list(components = c("facGender", "facExperim"))),
      descriptives = TRUE)
```



# Part III: Implementation Details

There are two aspects of the Syntax Mode implementation. Firstly, there is creation of the basic building blocks behind the Syntax Mode. Secondly, there is the interaction of the user with those building blocks. The following flow charts show a basic visual of these two aspects. of the implementation that allows module developers to interact with it and secondly there is the implementation that allows users to interact with it.
Creating the building blocks:

![image info](flow-chart-developer-syntax-mode.png)

User interaction with those building blocks:

![image info](flow-chart-user-syntax-mode.png)


## Creating the Syntax Mode Building Blocks

### Obtaining the Necessary Information
The R Obtains Class and wrapper can be automatically generated by an R script. However, this R script requires a summary of analysis.qml and Description.qml. Considering writing a good QML parser has kindly been performed for us by Qt it makes sense we use the JASP executable for this (through a shell command), or include a binary with jaspTools that can do this. Ideally this function will be callable from R so users can do most of their developing in RStudio. The output should be JSON that summarizes each component, with the following minimum information:

- Name
- Type (e.g., checkbox, radiobutton)
- Default value
- Validation checks (e.g., min, max)
- Info

<details>

<summary>Example summary of three components</summary>

Description.qml:
```
...
	"menu":
	[
		{
			"title":    "ANOVA",
			"function":	"anova"
		}
    ]
```

analysis.qml:
```
Form 
{
    info: "Allows the user to analyze the difference between multiple group means."

    AssignedVariablesList 
    { 
        name: "fixedFactors"
        title: qsTr("Fixed Factors")
        allowedColumns: ["ordinal", "nominal"]
        info: "The variables that are manipulated/define the different groups."
    }

    CheckBox 
    { 
        name: "descriptives";	
        label: qsTr("Descriptive statistics")
        info: "Descriptives for each combination of levels of the independent variables."
        childrenOnSameRow: true
        CIField { name: "ci"; info: "% confidence interval" }	
    }
}
```
Generated summary:
```
{
    "function": "anova",
    "info": "Allows the user to analyze the difference between multiple group means.",

    "options": 
    [
        {
            "name": "fixedFactors",
            "type": "variables",
            "allowed": ["ordinal", "nominal"],
            "default": "",
            "info": "The variables that are manipulated/define the different groups."
        },

        {
            "name": "descriptives",
            "type": "bool",
            "default": false,
            "info": "Descriptives for each combination of levels of the independent variables."
        },

        {
            "name": "ci",
            "type": "double",
            "min": 0,
            "max": 100,
            "default": 95,
            "info": "% confidence interval"
        }
    ]
}
```

Header of jasp-analysis-wrappers.R:
```
#' Allows the user to analyze the difference between multiple group means.
#'
#' @param fixedFactors The variables that are manipulated/define the different groups.
#' @param descriptives Descriptives for each combination of levels of the independent variables.
#' @param ci % confidence interval.
anova <- function(fixedFactors = "", descriptives = FALSE, ci = 95) {
    ...
}
```

</details>

#### Difficulties
- Problem with default values based on js logic; the qml needs to be run to figure out the true default configuration.

### The Wrapper
The wrapper is what users interact with when running the analysis syntax in RStudio or the JASP R editor. It's what handles the formula, translates the syntax back into an R options list and makes sure validation occurs by calling the Options Class. Furthermore, the wrapper should deal with the setup and breakdown of the jaspResults object.

<details>

<summary>Example of a naive wrapper that shows how the formula's can be used, but lacks validation</summary>

Example of calling the wrapper shown below, note that your working directory should be set to your `jaspAnova` clone:
```
if (require(jaspTools))
  bancova(Sepal.Length ~ Species + Sepal.Width, data = iris)
```

```
bancova <- function(formula           = NULL,
                    data              = NULL,
                    dep               = "",
                    fixed             = list(),
                    random            = list(),
                    covariates        = list(),
                    model             = list(),
                    nullModel         = NULL,
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
                    singleModel       = list(),
                    singleEst         = FALSE,
                    singleCriTable    = FALSE,
                    singlePostPlot    = FALSE,
                    singleQqPlot      = FALSE,
                    singleRsqPlot     = FALSE,
                    singleGroupPost   = "grouped",
                    postHocVars       = list(),
                    postHocNull       = TRUE,
                    descr             = FALSE,
                    descrVarOnYAxis   = "",
                    descrVarAsLines   = "",
                    descrVarAsPlots   = "",
                    wantPlotCi        = FALSE,
                    plotCi            = 0.95,
                    postPlot          = FALSE,
                    qqPlot            = FALSE,
                    rsqPlot           = FALSE,
                    fixedNumAcc       = 1,
                    fixedMCMCSamples  = 1,
                    sampleModeNumAcc  = "auto",
                    sampleModeMCMC    = "auto",
                    setSeed           = FALSE,
                    seed              = 1) {
  
  defaultArgCalls <- formals(as.character(match.call()[[1]]))
  defaultArgs <- lapply(defaultArgCalls, eval)
  
  options <- as.list(match.call())
  options <- modifyList(defaultArgs, options)
  options[["data"]] <- NULL
  if (!is.null(formula)) {
    
    if (!inherits(formula, "formula"))
      formula <- as.formula(formula)
    
    if (dep != "")
      formula <- addToFormula(formula, terms = dep, type = "lhs")
    if (length(fixed) > 0)
      formula <- addToFormula(formula, terms = fixed, type = "rhs")
    if (length(random) > 0)
      formula <- addToFormula(formula, terms = random, type = "rhs")
    if (length(covariates) > 0)
      formula <- addToFormula(formula, terms = covariates, type = "rhs")
    
    options$dep <- getFromFormula(formula, data, type = "lhs", mode = "numeric", exclude = random, nullValue = defaultArgs[["dep"]])
    options$fixed <- getFromFormula(formula, data, type = "rhs", mode = "factor", exclude = random, nullValue = defaultArgs[["fixed"]])
    options$covariates <- getFromFormula(formula, data, type = "rhs", mode = "numeric", exclude = random, nullValue = defaultArgs[["covariates"]])
    
    if (length(model) == 0)
      options$model <- createModelFromFormula(formula, type = "rhs", nuisance = list(name = "isNuisance", terms = nullModel))
    
    if (length(singleModel) == 0)
      options$singleModel <- createModelFromFormula(formula, type = "rhs")
  }
  
  options[["formula"]]   <- NULL
  options[["nullModel"]] <- NULL
  
  # this will go as name changes are permanent
  options <- convertOptionNames(options)
  # this will go when the jaspBase package defines all C++ objects
  jaspTools::runAnalysis("AncovaBayesian", dataset=data, options=options)
  jaspResults$print()
  
  # it might look a little like the following:
  #jaspResults <- initJaspResults()
  #jaspAnova:::.ancovaBayes(jaspResults=jaspResults, dataset=data, options=options)
  #summary(jaspResults)
  #return(invisible(jaspResults$asResults()))
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
                  descrVarOnYAxis   = "plotHorizontalAxis",
                  descrVarAsLines   = "plotSeparateLines",
                  descrVarAsPlots   = "plotSeparatePlots",
                  wantPlotCi        = "plotCredibleInterval",
                  plotCi            = "plotCredibleIntervalInterval",
                  postPlot          = "posteriorPlot",
                  qqPlot            = "qqPlot",
                  rsqPlot           = "rsqPlot",
                  fixedNumAcc       = "fixedNumAcc",
                  fixedMCMCSamples  = "fixedMCMCSamples",
                  sampleModeNumAcc  = "sampleModeNumAcc",
                  sampleModeMCMC    = "sampleModeMCMC",
                  setSeed           = "setSeed",
                  seed              = "seed")
  
  oldNames <- names(options)
  newNames <- oldNames
  for (i in seq_along(oldNames))
    if (oldNames[i] %in% names(mapping))
      newNames[i] <- mapping[[oldNames[i]]]
  
  names(options) <- newNames
  
  return(options)
}
```

</details>

#### The Wrapper: thoughts
- Wrapper should be able to call the column encoding functionality.
- How should the name of the wrapper differ from the analysis fn name? Perhaps: anova and .anova (wrapper, analysis, respectively).
- Problem with analyses that have a whole host of options appearing and disappearing e.g., network analysis that must all be present in the wrapper and it might become difficult to call.
- All options should be renamed to something shorter that still makes sense. Everything needs to be added to the upgrader.
- If we add the help from our helpfiles (accessible through e.g., `?jasp::banova`), then it won't always be clear to the user what the actual R input should be. For example, from ttestpairedsamples.md we obtain the following info about `variables`: `In this box the variables are selected for which the difference is computed. Multiple differences can be analysed at the same time by specifying different rows with two variables for which the difference is computed. In other words, each row represents other difference scores."` which does not relay that it should be `list(c("var1", "var2"))`.

#### The Formula
The formula is basically a summary of multiple `AssignedVariableList`s together with the model terms component. Formula's consist of left hand side (lhs) and right hand side (rhs) operators. The lhs are dependent variables that are `+` separated. The rhs are independent variables that are also `+` separated; rhs terms may also contain interactions of multiple independent variables, specified by `*`.

To specify formula's we need to introduce two properties to the qml: `positionInFormula` (`"lhs"` or `"rhs"`), `mustSpecify` (boolean, see below in the thoughts section) and `modelSpecification` (boolean). `positionInFormula` can be used to specify where the terms need to go in a formula. `modelSpecification` needs to be set to `true` if an `AssignedVariablesList` exists that dictates which rhs terms are included in the model (this is the case in the ANOVA's and Regression's); the terms included in this `AssignedVariablesList` are a superset of the individual fields with the  `"rhs"` specification and so can be used directly.

<details>

<summary>Example of specifying a formula in qml</summary>

```
VariablesForm
{
    AvailableVariablesList { name: "allVariablesList" }
    AssignedVariablesList  { name: "dependent";		title: qsTr("Dependent Variable");	positionFormula: "lhs"; suggestedColumns: ["scale"]; singleVariable: true;          }
    AssignedVariablesList  { name: "fixedFactors";	title: qsTr("Fixed Factors");		positionFormula: "rhs"; suggestedColumns: ["ordinal", "nominal"];                   }
    AssignedVariablesList  { name: "randomFactors";	title: qsTr("Random Factors");		positionFormula: "rhs"; suggestedColumns: ["ordinal", "nominal"]; mustSpecify: true }
    AssignedVariablesList  { name: "wlsWeights";	title: qsTr("WLS Weights");	                                suggestedColumns: ["scale"]; singleVariable:                }
}
```

This analysis has model terms so we specify this is the case and consequently the rhs of the formula will be populated by this field:
```
VariablesForm
{
    AvailableVariablesList { name: "components"; title: qsTr("Components"); source: ["fixedFactors", "randomFactors"]                 }
    AssignedVariablesList  { name: "modelTerms"; title: qsTr("Model Terms"); listViewType: JASP.Interaction; modelSpecification: true }
}

Note that despite the existence of `modelSpecification: true`, it is still necessary to specify the `rhs` terms separately in the qml, because otherwise it won't be possible to know which terms are to be excluded from the function call in favour of the formula.
```

</details>

##### The Formula: thoughts
- It needs to remain possible to uniquely identify which independent variable in the formula comes from which box (e.g., fixed factor, random factor, covariate). The distinction between fixed/random factors on the one hand and covariates on the other can be made based on type (factor vs numeric), but the distinction between fixed and random factors cannot be made -- they are both factors. In this case the random factor needs a property `mustSpecify` that signifies that it must always be specified in addition to the formula (e.g., `formula = ~ contBinom + facGender, random = contBinom)`).
- The inclusion of additional properties in the model terms (e.g., "include in null"). So far This has always been a single checkbox and can be solved by having two formula's, one that specifies the full model and one that specifies which are null terms (e.g., `formula = ~ contBinom + facGender, nullModelFormula = ~ contBinom)`). However, this will not work as easily with anything other than a checkbox and in those cases it might be necessary to disallow formula's.
- Can analyses have multiple formula's?
- Where to add the additional parameters `formula` and `nullModel` in the options list? Model terms or keep them as additional arguments in the analysis too?

### The Options Class
The Options Class plays two parts. Firstly, it returns the R syntax for a given set of JSON options in JASP and secondly it validates and returns R options for supplied syntax to the wrapper. The idea is to create an R6 `jaspOptions` class in `jaspBase` which defines logic for each component type (checkbox, radiobutton). The generated Options Class can then inherit from `jaspOptions` and apply the parent logic to all the qml components for a specific analysis. `jaspOptions` should implement:
- `jaspOptions$new(options)`
- `jaspOptions$validate()`
- `jaspOptions$toSyntax()`

#### The Options Class: thoughts
- Should validation based on javascript be incorporated (e.g., the `min` of a component depends on a different component's value)?
- Should the enabled status of components be taken into account?
- In addition to basic validation (e.g., `ciLevel < 1`), every component should be checked for what may be its "missing value" (e.g., `list()`) and its "filled value" (e.g., `list(list(component = "contBinom"))`) to ensure the correctness of the supplied value.

## User interaction with the Syntax Mode within JASP

### Displaying Syntax in JASP
There needs to be an area in JASP where the user can find the syntax that was generated by their option selection. Considering syntax is a description of the input of an analysis a natural place for this would be to be in the QML input panel. We can add an additional button on top of an analysis panel that lets users toggle between clickable options and the syntax. The syntax will be fetched by querying the Options Class.

### Running Syntax in the JASP R Editor
At the bare minimum the syntax should give results in our R editor. The data is already loaded, the C++ objects are present and all packages are loaded. So the focus should be on making sure that the results make sense in an R console. This means:
- Creating a summary method with a very good looking ASCII print; either we should work on our current implementation or use something like the [stargazer](https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf) package:
```
> stargazer::stargazer(head(iris), type = "text")

===========================================================
Statistic    N Mean  St. Dev.  Min  Pctl(25) Pctl(75)  Max 
-----------------------------------------------------------
Sepal.Length 6 4.950  0.288   4.600  4.750    5.075   5.400
Sepal.Width  6 3.383  0.343   3.000  3.125    3.575   3.900
Petal.Length 6 1.450  0.138   1.300  1.400    1.475   1.700
Petal.Width  6 0.233  0.082   0.200  0.200    0.200   0.400
-----------------------------------------------------------
```

- Making sure the output is more 'sane'; the excessive nesting should be reduced to top level elements.
- Index names must be short and to the point, instead of the long concatenations.
- Tables need to be a common format like a regular data.frames or the like that allows users to easily use them for further analysis.
- Plots should go to the plot device and the plot object returned in the results list (and the R editor should be able to handle plots without crashing).
- `info(jasp::banova(...args...))` could be used to return additional info in an enriched ASCII representation of the analysis. Each output element will have info added next to it; these info elements can be extracted from the `$addInfo`'s used in the analysis.

### Running Syntax in the JASP R Editor: thoughts
- How should a table with collapsed columns be represented in R?

## User interaction with the Syntax Mode within RStudio

### Package Eco-system
To make it easy to perform an analysis in R it needs to be simple to install our R packages and run them. They should not require anything from JASP.
A number of packages need to be created with distinct sets of functionality. Doing so, will ensure people can check the namespace and only find a coherent set of functions. Furthermore, this will ensure we can easily update separate components, without users having to reinstall everything when something unrelated to what they do (i.e., develop, or run analyses) changes.
- `jasp`: A package that bundles the common R analyses in one namespace. Installs all the required R analysis packages that are created by the JASP team. Exports only the analysis functions and the functions for the filter and column computation. The benefit of this approach is that users only need to know about a single package, only need to install packages once and it becomes clear from looking at the `jasp` namespace which analyses are available. Additional modules can be installed as separate R packages.
- `jaspBase`: A package that contains all the building blocks for creating a JASP R analysis. This is basically a combination of jaspResults and the convenience functions such as those found in common.R and commonerrorcheck.R.
- `jaspGraphs`: A package that handles all our plotting needs.
- `jaspTools`: A package that aids an R analysis developer with the creation of their JASP R analysis. It exports functions related to the creation of an JASP-R package skeleton, the handling of translations, creation of a wrapper, etc.

### Running environment
At present jaspTools is required to run an analysis, this is mainly because JASP inserts certain objects into the global R environment, which naturally do not exist when run outside of JASP. To ensure analyses can be run from R:
- The `jaspBase` package needs to provide drop-in replacements for the C++ functions defined in `jasprcpp.cpp` (e.g., `encodeColNamesStrict`).
- The `jaspBase` package needs to make column encoding available to ensure that when an analysis works in JASP it will also work in R.
- The `jaspBase` package needs to make the `jaspResults` object assignable to a variable, in this way users can run multiple analyses.
- Each analysis must be able to correctly deal with receiving data directly through the `dataset` argument.