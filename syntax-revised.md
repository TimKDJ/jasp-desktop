## Part I: Definition and Goals

### Definition
Syntax mode refers to the ability for users to see what code generates the output they see. This includes filters, computed columns and analyses.

### Use Case
1. Syntax mode will provide a smoother transition from SPSS that makes heavy use of syntax.
2. With syntax mode it becomes easier to develop new R analyses as they can be run from RStudio with little effort.
3. Syntax can easily be shared between colleagues and attached to appendices.
4. JASP can be a tool to expose students to R.

### Primary Goals of Syntax Mode
1. The syntax will cover filters, computed columns and analyses.
2. The syntax will be valid R and can be run outside of JASP.
3. The syntax will be consistent with the syntax you would provide to regular R packages (e.g., `stats::lm()`).
4. The user will need to perform minimal additional steps after copy-pasting to generate output similar of that seen in JASP (i.e., only read data).
5. The output of the syntax in RStudio will generate a structure that R users are familiar with.
6. The syntax is set up in a way that will, **in the future**, allow users to execute syntax inside of JASP and have JASP perform the specified filters/computed columns/analyses.

### Intended Limitations of Syntax Mode
1. The syntax does not need to be executable in JASP at present, but we'll likely want that at a later stage.
2. The syntax can be separate statements that are not linked together. What this means is that it is acceptable not to include package installing, data reading and temporary variables (although we could add this as an additional feature). By doing this we will ensure it will be easier to later execute our generated syntax inside of JASP (once we reach that point).
   
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

## Part II: Examples of Intended Syntax

### Filters
Based on `dplyr::filter()`, with only a small adjustment to their logic: allow no data to be passed in, in which case we search for a dataset in the `parent.frame`. This ensures we don't have to enter `dplyr::filter(data = dataset, ...)` everywhere. The following filter types are already supported by `dplyr::filter()` and return filtered `data.frame`s:
```
# filter from "drag 'n drop interface"
filter(contNormal < 0)


# filter from "R interface"
filter({
    temp1 <- facExperim
    temp1 == 1 & contBinom == 0
})
```

### Computed Columns
Based on `dplyr::mutate()`, with the same proposed change of removing `data` (and a rename of the function to `compute`). The following ways of column adding are already supported by `dplyr::mutate()` and return `data.frame`s with the additional columns:
```
# single new column titled 'sumScore' created from "drag 'n drop interface"
compute(sumScore = contNormal + contGamma)


# two new columns titled 'logContNormal' and 'expContGamma' (we don't support this in JASP)
compute(logContNormal = log(contNormal), expContGamma = contGamma^2)


# single new column titled 'sumScore' created from "R interface"
compute(logSumScore = {
  sumScore <- contNormal + contGamma
  log(sumScore)
})
```

### Analyses
These functions are intended to be alike to statistical functions such as `stats::lm()`, especially in regards to the use of formula's. JASP analysis functions should only deviate from other R functions when it comes to output. Whereas JASP will creates lots of output from a single function call, regular R functions generally only do "one thing". However, our approach might actually be beneficial as it requires the users to only know about one single function. To accomplish a clean interface for the analyses, we will need wrappers around the existing analyses. This is an example call to a wrapper around our ANOVA analysis:
```
# calling an anova with a formula
anova(formula = contNormal ~ facGender + facExperim + facGender * facExperim,
      descriptives = TRUE)
```

##### Formula's
Formula's complicate the implementation, but simplify the syntax. Furthermore, they are the standard way in which models are specified in R packages. The next example shows the benefit of formula's by repeating the ANOVA call, but without using a formula:
```
# calling an anova without a formula
anova(dep = "contNormal",
      fixed = c("facGender", "facExperim"),
      modelTerms = list(list(components = "facGender"), list(components = "facExperim"), list(components = c("facGender", "facExperim"))),
      descriptives = TRUE)
```


## Part III: Implementation Details

### Generating a Wrapper Around an Analysis
We need an automatically generated wrapper around the main analysis function. This wrapper should deal with the setup and breakdown of jaspResults, as well as the validation of provided options.
-    Thin wrapper around analysis that creates the options list and initializes jaspResults
-    Needs to be possible to generate a json representation of the options
    -    Formula’s
    -    Validation
-    Need a R6 options class (min, max, allowedColumns)
    -    Be able to be called from R so developers can create a wrapper during module creation

### Generating Syntax Based on Selected Input Options in JASP
-    Utilize options R6 class, simply supply normal options json

### Displaying Syntax in JASP
-    Options:
    -    QML input panel, above click options
    -    New separate panel
    -    In the output panel

### Running an Analysis in RStudio
To make it easy to perform an analysis in R it needs to be simple to install our R packages and run them. They should not need anything from JASP.

#### Package Eco-system
A number of packages need to be created with distinct sets of functionality. Doing so, will ensure people can check the namespace and only find a coherent set of functions (e.g., `descriptives()` and `reliability()` will be together, but not `hasErrors()`). Furthermore, this will ensure we can easily update separate components, without users having to reinstall everything when something unrelated to what they do (i.e., develop, or run analyses) changes.
- `jasp`: A package that bundles all the R analyses in one namespace. Installs all the required R analysis packages that are created by the JASP team. Exports only the analysis functions and the functions for the filter and column computation. The benefit of this approach is that users only need to know about a single package, only need to install packages once and it becomes clear from looking at the `jasp` namespace which analyses are available. External modules can be installed as separate R packages.
- `jaspAnalysis` (or `jaspCore`, `jaspBackbone`, ...): A package that contains all the building blocks for creating a JASP R analysis. This is basically a combination of jaspResults and the convenience functions such as those found in common.R and commonerrorcheck.R.
- `jaspGraphs`: A package that handles all our plotting needs.
- `jaspTools`: A package that aids an R analysis developer with the creation of their JASP R analysis. It exports functions related to the creation of an JASP-R package skeleton, the handling of translations, creation of a wrapper, etc.

#### Analysis Environment
The `jaspAnalysis` package needs to
-    jaspResults needs to be kept within package
-   automatically inserted functions should be inserted from JASP

#### Result Structure
-    Results structure should not be so nested

#### Plots
-    Plots go to plotviewer

#### Tables
-    Tables need to be prettier ASCII (there is some package that adds a footnote)
-    Tables need more normal names

Problems:
-    Can analyses have multiple formulas?
-    Where to add formula and isNuisance in the options list? Model terms?


