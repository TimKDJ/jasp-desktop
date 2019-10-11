.onAttach <- function(libname, pkgname) {
	env <- globalenv()

	if (exists("jaspResults", env)) {
		destroyAllAllocatedObjects()
		destroyAllAllocatedRObjects()
		rm(list=ls(envir=.plotStateStorage), envir=.plotStateStorage)
	}

	env$jaspResults <- jaspResultsR$new(create_cpp_jaspResults("Analysis Test", NULL))
	
	return(invisible(TRUE))
}

#For use inside jaspResults to store plots and states (as is obvious from the name)
.plotStateStorage <- new.env()

initJaspResults <- function() .onAttach()

startProgressbar <- function(expectedTicks, label="") {
	if (!is.numeric(expectedTicks) || !is.character(label))
		stop("`expectedTicks` must be numeric and `label` a character")
	if (nchar(label) > 40)
		stop("The label must be 40 characters at most")
		
	if (jaspResultsCalledFromJasp())
		jaspResultsModule$cpp_startProgressbar(expectedTicks, label)
	else
		cpp_startProgressbar(expectedTicks, label)
}

progressbarTick <- function() { 
	if (jaspResultsCalledFromJasp())
		jaspResultsModule$cpp_progressbarTick()
	else
		cpp_progressbarTick()
}

checkForJaspResultsInit <- function() {if (!exists("jaspResults", .GlobalEnv)) .onAttach()}

is.JaspResultsObj <- function(x) {
	inherits(x, "R6") && 
  inherits(x, c("jaspResultsR", "jaspContainerR", "jaspObjR", "jaspOutputObjR", "jaspPlotR", "jaspTableR", "jaspHtmlR", "jaspStateR", "jaspColumnR"))
}

destroyAllAllocatedRObjects <- function() {

	# some attempt to clear out R objects with invalid pointers	
	s <- search()
	envs2Search <- s[!(startsWith(s, "package:") | startsWith(s, "tools:") | s == "Autoloads")]
	
	for (envName in envs2Search) {
		
		nms2rm <- character()
		env <- as.environment(envName)
		
		for (n in names(env)) {
			if (is.JaspResultsObj(env[[n]])) {
				
				# check if externalpoint of object is invalid
				if (isTRUE(try(silent = TRUE, identical(
					env[[n]]$.pointer,
					new("externalptr")
					)))) {
					nms2rm <- c(nms2rm, n)
				}
			}
		}
		# delete objects from env
		if (length(nms2rm) > 0)
				rm(list = nms2rm, envir = env)
	}
}

jaspResultsCalledFromJasp <- function() {
  # a variety of tests to check if a createJasp*() function is called from JASP
  return(
    exists("jaspResultsModule", mode = "S4") && 
      inherits(jaspResultsModule, "Module") && 
      identical(slotNames(jaspResultsModule), ".xData")
  )
}

extractErrorMessage <- function(error) {
  splits <- unlist(strsplit(as.character(error), ":", fixed=TRUE))
  errorMsg <- splits[length(splits)]
  return(trimws(errorMsg))
}

createJaspPlot <- function(plot=NULL, title="", width=320, height=320, aspectRatio=0, error=NULL, dependencies=NULL, position=NULL)
	return(jaspPlotR$new(plot = plot, title = title, width = width, height = height, aspectRatio = aspectRatio, error = error, dependencies = dependencies, position = position))

createJaspContainer <- function(title="", dependencies=NULL, position=NULL)
	return(jaspContainerR$new(title = title, dependencies = dependencies, position = position))

createJaspTable <- function(title="", data=NULL, colNames=NULL, colTitles=NULL, overtitles=NULL, colFormats=NULL, rowNames=NULL, rowTitles=NULL, dependencies=NULL, position=NULL)
	return(jaspTableR$new(title = title, data = data, colNames = colNames, colTitles = colTitles, overtitles = overtitles, colFormats = colFormats, rowNames = rowNames, rowTitles = rowTitles, dependencies = dependencies, position = position))

createJaspHtml <- function(text="", elementType="p", class="", dependencies=NULL, title="hide me", position=NULL)
	# if you change "hide me" here then also change it in Common.R and in HtmlNode.js or come up with a way to define it in such a way to make it show EVERYWHERE...
	return(jaspHtmlR$new(text = text, elementType = elementType, class = class, dependencies = dependencies, title = title, position = position))

createJaspState <- function(object=NULL, dependencies=NULL)
  return(jaspStateR$new(object = object, dependencies = dependencies))

createJaspColumn <- function(columnName="", dependencies=NULL)
  return(jaspColumnR$new(columnName = columnName, dependencies = dependencies))

# also imported but that doesn't work in JASP
R6Class <- R6::R6Class

# inheritance structure:
# 1. jaspResults	->	-
# 2. jaspObj			->	2.1. jaspState
#									->	2.2. jaspOutputObj	->	2.2.1. jaspHtml
#																					->	2.2.2. jaspContainer
#																					->	2.2.3. jaspPlot
#																					->	2.2.4. jaspTable
#																					->	2.2.5. jaspColumn

# R6 definitions

jaspResultsR <- R6Class(
	classname = "jaspResultsR",
	cloneable = FALSE,
	public    = list(
		initialize = function(x) {
			if (!missing(x) && isS4(x) && inherits(x, "Rcpp_jaspResultsClass"))
				private$jaspObject = x
      else if (inherits(x, "jaspResultsR")) # this if is needed because JASP and R call jasprResults in different ways
				private$jaspObject = private$getJaspObject(x)
			else
			  stop("You should not create a new jaspResultsR object!")
		},

		addCitation = function(x) {
			if (!is.character(x)) 
				stop("Citation must be a character (vector)")
			for (i in seq_along(x))
				private$jaspObject$addCitation(x[i])
		},
		print           = function()	private$jaspObject$print(),
		printHtml       = function()	private$jaspObject$printHtml(),
		setError        = function(x)	private$jaspObject$setError(x),
		getError        = function()	private$jaspObject$getError()
	),
	private = list(
		children    = list(),
		jaspObject  = NULL,
		jaspCppToR6 = function(cppObj) {
			return(switch(
				class(cppObj),
				"Rcpp_jaspPlot"      = jaspPlotR$new(jaspObject = cppObj),
				"Rcpp_jaspTable"     = jaspTableR$new(jaspObject = cppObj),
				"Rcpp_jaspContainer" = jaspContainerR$new(jaspObject = cppObj),
        "Rcpp_jaspColumn"    = jaspColumnR$new(jaspObject = cppObj),
        "Rcpp_jaspState"     = jaspStateR$new(jaspObject = cppObj),
				"Rcpp_jaspHtml"      = jaspHtmlR$new(jaspObject = cppObj),
				stop(sprintf("Invalid call to jaspCppToR6. Expected jaspResults object but got %s", class(cppObj)))
			))
		},
		setField	= function(field, value) {
			private$jaspObject[[field]] <- private$getJaspObject(value);
			private$children[[field]]   <- value;
		},
		getField	= function(field) {
			#maybe changing the dependencies removed this object when we weren't looking!
			if (is.null(private$jaspObject[[field]]) && !is.null(private$children[[field]]))
				private$children[[field]] <- NULL

			#other way 'round is also quite possible, we just regenerated jaspResults from state/json and now the R6 class doesn't know anything about it...
			if (!is.null(private$jaspObject[[field]]) && is.null(private$children[[field]]))
				private$children[[field]] <- private$jaspCppToR6(private$jaspObject[[field]])

			return(private$children[[field]])
		},
		getJaspObject           = function(R6obj)   R6obj$.__enclos_env__$private$jaspObject,
		getResults              = function()        private$jaspObject$getResults(),
		setOptions              = function(options) private$jaspObject$setOptions(options),
		send                    = function()        private$jaspObject$send(),
		setErrorMessage         = function(msg)     private$jaspObject$setErrorMessage(msg),
		changeOptions           = function(options) private$jaspObject$changeOptions(options),
		getKeepList             = function()        private$jaspObject$getKeepList(),
		complete                = function()        private$jaspObject$complete(),
		getPlotObjectsForState  = function()        private$jaspObject$getPlotObjectsForState(),
    getOtherObjectsForState = function()        private$jaspObject$getOtherObjectsForState()
	),
	active = list(
	  status = function(x) { if (missing(x)) private$jaspObject$status else private$jaspObject$status <- x }
	)
)

`[[<-.jaspResultsR` <- function(x, field, value) {
	x$.__enclos_env__$private$setField(field, value)
	return(x)
}
`[[.jaspResultsR`   <- function(x, field)
	x$.__enclos_env__$private$getField(field)
	
print.jaspResultsR <- function(x, ...) 	# TODO: make this a pretty summary print (But please do this in std::string jaspObject::toString() and the overrides)
	x$print()

jaspObjR <- R6Class(
	classname = "jaspObjR", 
	cloneable = FALSE,
	public    = list(
		initialize = function()	stop("You should not create a new jaspObject!"),
		print      = function()	private$jaspObject$print(),
		dependOn   = function(options=NULL, optionsFromObject=NULL, optionContainsValue=NULL) {
			if (!is.null(options)) {
				if (!is.character(options))
					stop("please provide a character vector in `options`")
				private$jaspObject$dependOnOptions(options)
			}
			
			if (!is.null(optionsFromObject)) {
				if (is.JaspResultsObj(optionsFromObject)) {
					private$jaspObject$copyDependenciesFromJaspObject(private$getJaspObject(optionsFromObject))
				} else if (is.list(optionsFromObject)) {
					for (object in optionsFromObject)
						if (is.JaspResultsObj(object))
							private$jaspObject$copyDependenciesFromJaspObject(private$getJaspObject(object))
				} else {
					stop("please provide a (list of) jasp object(s) in `optionsFromObject`")
				}
			}
				
			if (!is.null(optionContainsValue)) {
				if (!is.list(optionContainsValue) || is.null(names(optionContainsValue)))
					stop("please provide a named list in `optionContainsValue`")
				for (i in seq_along(optionContainsValue)) {
					name <- names(optionContainsValue)[i]
					value <- optionContainsValue[[i]]
					private$jaspObject$setOptionMustContainDependency(name, value)
				}
			}
		}
	),
	private	= list(
		jaspObject    = NULL,
		getJaspObject = function(R6obj) R6obj$.__enclos_env__$private$jaspObject
	)
)

print.jaspObjR <- function(x, ...) 	# TODO: print actual information depending on object type
	x$print()

jaspStateR <- R6Class(
	classname = "jaspStateR",
	inherit   = jaspObjR,
	cloneable = FALSE,
	public    = list(
		initialize = function(object=NULL, dependencies=NULL, jaspObject=NULL) {
			if (is.null(jaspObject)) {
			  if (jaspResultsCalledFromJasp()) {
				  jaspObject <- jaspResultsModule$create_cpp_jaspState("")
			  } else {
				  checkForJaspResultsInit()
				  jaspObject <- create_cpp_jaspState("")
			  }
      }
      private$jaspObject <- jaspObject
      
      if (!is.null(object))
				self$object <- object
			
			if (!is.null(dependencies))
				self$dependOn(dependencies)
		}
	),
	active = list(
		object = function(x) { if (missing(x)) private$jaspObject$object else private$jaspObject$object <- x }
	)
)

jaspOutputObjR <- R6Class(
	classname = "jaspOutputObjR",
	inherit   = jaspObjR,
	cloneable = FALSE,
	public    = list(
		initialize  = function()	stop("You should not create a new jaspOutputObject!"),
		printHtml   = function()	private$jaspObject$printHtml(),
		setError    = function(x)	private$jaspObject$setError(x),
		getError    = function()	private$jaspObject$getError(),
		addCitation = function(x) {
			if (!is.character(x)) 
				stop("Citation must be a character (vector)")
			for (i in seq_along(x))
				private$jaspObject$addCitation(x[i])
		}
	),
	active	= list(
		position = function(x) { if (missing(x)) private$jaspObject$position else private$jaspObject$position <- as.numeric(x) },
		title    = function(x) { if (missing(x)) private$jaspObject$title    else private$jaspObject$title    <- x }
	),
	private = list(
	  handleErrors = function(data) { if (inherits(data, "try-error")) private$jaspObject$setError(extractErrorMessage(data)) else return(data) }
	)
)

jaspHtmlR <- R6Class(
	classname = "jaspHtmlR",
	inherit   = jaspOutputObjR,
	cloneable = FALSE,
	public    = list(
		initialize = function(text="", elementType="p", class="", dependencies=NULL, title="hide me", position=NULL, jaspObject = NULL) {
			# if you change "hide me" here then also change it in Common.R and in HtmlNode.js or come up with a way to define it in such a way to make it show EVERYWHERE...
			if (is.null(jaspObject)) {
			  if (jaspResultsCalledFromJasp()) {
				  jaspObject <- jaspResultsModule$create_cpp_jaspHtml("")
			  } else {
				  checkForJaspResultsInit()
				  jaspObject <- create_cpp_jaspHtml("")
        }
			}
      private$jaspObject <- jaspObject
			
      self$text        <- text
			self$elementType <- elementType
			self$class       <- class
			self$title       <- title
			
			if (!is.null(dependencies))
				self$dependOn(dependencies)
			
			if (is.numeric(position))
				self$position <- position
		}
	),
	active = list(
		text        = function(value) { if (missing(value)) private$jaspObject$text        else private$jaspObject$text        <- value },
		class       = function(value) { if (missing(value)) private$jaspObject$class       else private$jaspObject$class       <- value },
		elementType = function(value) { if (missing(value)) private$jaspObject$elementType else private$jaspObject$elementType <- value }
	)
)

jaspContainerR <- R6Class(
	classname = "jaspContainerR",
	inherit   = jaspOutputObjR,
	cloneable = FALSE,
	public    = list(
		initialize = function(title = "", dependencies = NULL, position = NULL, jaspObject = NULL) {
			if (is.null(jaspObject)) {
        if (jaspResultsCalledFromJasp()) {
          jaspObject <- jaspResultsModule$create_cpp_jaspContainer("")
        } else {
          checkForJaspResultsInit()
          jaspObject <- create_cpp_jaspContainer("") # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspContainer, title))
        }
      }
			private$jaspObject <- jaspObject
      
      self$title <- title
      
			if (!is.null(dependencies))
				self$dependOn(dependencies)
			
			if (is.numeric(position))
				self$position <- position
		}
	),
	private	= list(
		children    = list(),
		jaspObject  = NULL,
		jaspCppToR6 = function(cppObj) {
			return(switch(
				class(cppObj),
				"Rcpp_jaspPlot"      = jaspPlotR$new(jaspObject = cppObj),
				"Rcpp_jaspTable"     = jaspTableR$new(jaspObject = cppObj),
				"Rcpp_jaspContainer" = jaspContainerR$new(jaspObject = cppObj),
        "Rcpp_jaspColumn"    = jaspColumnR$new(jaspObject = cppObj),
				"Rcpp_jaspState"     = jaspStateR$new(jaspObject = cppObj),
				"Rcpp_jaspHtml"      = jaspHtmlR$new(jaspObject = cppObj),
				stop(sprintf("Invalid call to jaspCppToR6. Expected jaspResults object but got %s", class(cppObj)))
			))
		},
		setField   = function(field, value) {
			private$jaspObject[[field]] <- private$getJaspObject(value);
			private$children[[field]]   <- value;
		},
		getField   = function(field) {
			#maybe changing the dependencies removed this object when we weren't looking!
			if (is.null(private$jaspObject[[field]]) && !is.null(private$children[[field]]))
				private$children[[field]] <- NULL

			#other way 'round is also quite possible, we just regenerated jaspResults from state/json and now the R6 class doesn't know anything about it...
			if (!is.null(private$jaspObject[[field]]) && is.null(private$children[[field]]))
				private$children[[field]] <- private$jaspCppToR6(private$jaspObject[[field]])

			return(private$children[[field]]);
		}
	)
)

`[[<-.jaspContainerR` <- function(x, field, value) {
	x$.__enclos_env__$private$setField(field, value)
	return(x)
}
`[[.jaspContainerR`   <- function(x, field)
	x$.__enclos_env__$private$getField(field)

initializeJaspObject <- function(withinJaspInitializer, outsideJaspInitializer, jaspObject, ...) {
  if (!is.null(jaspObject))
    return(jaspObject)
  
  if (jaspResultsCalledFromJasp()) {
    jaspObject <- withinJaspInitializer(...)
  } else {
    checkForJaspResultsInit()
    jaspObject  <- outsideJaspInitializer(...) # If we use R's constructor it will garbage collect our objects prematurely..
  }
  
  return(jaspObject)
}
	
jaspPlotR <- R6Class(
	classname = "jaspPlotR",
	inherit   = jaspOutputObjR,
	cloneable = FALSE,
	public    = list(
		initialize = function(plot=NULL, title="", width=320, height=320, aspectRatio=0, error=NULL, 
							  dependencies=NULL, position=NULL, jaspObject = NULL) {
			if (is.null(jaspObject)) {
			  if (jaspResultsCalledFromJasp()) {
				  jaspObject <- jaspResultsModule$create_cpp_jaspPlot("")
			  } else {
				  checkForJaspResultsInit()
				  jaspObject  <- create_cpp_jaspPlot("") # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspPlot, title)
			  }
			}
      private$jaspObject <- jaspObject
      
			if (aspectRatio > 0 && !is.null(width) && width != 0)
				height <- aspectRatio * width
			else if (aspectRatio > 0)
				width <- height / aspectRatio
			
			self$width  <- width
			self$height <- height
			self$aspectRatio <- aspectRatio		
      self$title <- title
			
			if (!is.null(error))
				self$setError(error)
			
			if (!is.null(plot))
				self$plotObject <- plot
			
			if (!is.null(dependencies))
				self$dependOn(dependencies)
			
			if (is.numeric(position))
				self$position <- position
		}
	),
	active = list(
		plotObject  = function(x) if (missing(x)) private$jaspObject$plotObject   else private$jaspObject$plotObject   <- private$handleErrors(x),
		aspectRatio = function(x) if (missing(x)) private$jaspObject$aspectRatio  else private$jaspObject$aspectRatio  <- x,
		width       = function(x) if (missing(x)) private$jaspObject$width        else private$jaspObject$width        <- x,
		height      = function(x) if (missing(x)) private$jaspObject$height       else private$jaspObject$height       <- x,
		status      = function(x) if (missing(x)) private$jaspObject$status       else private$jaspObject$status       <- x
	)
)

jaspTableR <- R6Class(
	classname = "jaspTableR",
	inherit   = jaspOutputObjR,
	cloneable = FALSE,
	public    = list(
		initialize = function(title="", data=NULL, colNames=NULL, colTitles=NULL, overtitles=NULL, colFormats=NULL, rowNames=NULL, rowTitles=NULL, dependencies=NULL, position=NULL, jaspObject=NULL) {
			if (is.null(jaspObject)) {
        if (jaspResultsCalledFromJasp()) {
          jaspObject <- jaspResultsModule$create_cpp_jaspTable("")
        } else {
          checkForJaspResultsInit()
          jaspObject <- create_cpp_jaspTable("") # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspTable, title)
        }
      }
      private$jaspObject <- jaspObject
      
      self$title <- title
      
      if (!is.null(data))
				self$setData(data)
			
			if (!is.null(colNames))
				self$setColNames(colNames)
      
#       if (!is.null(rowNames))
#         self$setRowNames(rowNames)
#       
#       if (!is.null(rowTitles))
#         self$setRowTitles(rowTitles)
# 			
# 			if (!is.null(colTitles))
# 				self$setColTitles(colTitles)
# 			
# 			if (!is.null(overtitles))
# 				self$setColOvertitles(overtitles)
# 			
# 			if (!is.null(colFormats))
# 				self$setColFormats(colFormats)
			
			if (!is.null(dependencies))
				self$dependOn(dependencies)
			
			if (is.numeric(position))
				self$position <- position
		},
		addColumns  = function(cols) private$jaspObject$addColumns(cols),
		setData     = function(data) private$jaspObject$setData(private$handleErrors(data)),
		addFootnote = function(message = "", symbol = NULL, colNames = NULL, rowNames = NULL) {
			if (is.null(colNames) && is.null(rowNames) && is.null(symbol)
					&& !grepl("^<.*?>note\\.?</.*?>", message, ignore.case=TRUE))
				symbol <- "<em>Note.</em>"
			private$jaspObject$addFootnoteHelper(message, symbol, colNames, rowNames)
		},
		addColumnInfo = function(name = NULL, title = NULL, overtitle = NULL, type = NULL, format = NULL, combine = NULL) {
			if (!is.null(type)) {
				permittedTypes <- c("integer", "number", "pvalue", "string")
				if (!type %in% permittedTypes)
					stop("type must be ", paste0("`", permittedTypes, "`", collapse=", "), " (provided type: `", type, "`)")
				if (is.null(format) && type == "number")
					format <- "sf:4;dp:3"
				else if (type == "pvalue")
					format <- "dp:3;p:.001"
			}
			private$jaspObject$addColumnInfoHelper(name, title, type, format, combine, overtitle)
    },
    addRows = function(rows, rowNames = NULL) {
      
      rows <- private$handleErrors(rows) # TODO: make it add a footnote if it's only one errored row in between otherwise good data

      maxElementLength <- 0 # Lets check if the users means a single row...
      if(is.list(rows) & !is.data.frame(rows))  maxElementLength <- max(unlist(lapply(rows, length)))
      else if(is.vector(rows))                  maxElementLength <- 1

      if(maxElementLength == 1)
      {
        if (is.null(rowNames))    private$jaspObject$addRow(rows)
        else                      private$jaspObject$addRow(rows, rowNames)
      }
      else
      {
        if (is.null(rowNames))    private$jaspObject$addRows(rows)
        else                      private$jaspObject$addRows(rows, rowNames)
      }
    },
		setExpectedSize = function(rows=NULL, cols=NULL) {
			inputTypes <- c(mode(rows), mode(cols))

      if (!all(inputTypes %in% c("numeric", "NULL")))	stop("Please use numeric values to set the expected size")

      if (!is.null(rows) && !is.null(cols))		private$jaspObject$setExpectedSize(cols, rows)
      else if (!is.null(rows))        				private$jaspObject$setExpectedRows(rows)
      else if(!is.null(cols))         				private$jaspObject$setExpectedColumns(cols)
      else                                    stop("Enter cols, rows or both in setExpectedSize!")
    },
    getColumnName       = function(columnIndex)               { return( private$jaspObject$colNames           [[columnIndex]]);               },
    setColumnName       = function(columnIndex, newName)      {         private$jaspObject$colNames$insert(     columnIndex,  newName);       },
    getColumnTitle      = function(columnName)                { return( private$jaspObject$colTitles          [[columnName]]);                },
    setColumnTitle      = function(columnName, newTitle)      {         private$jaspObject$colTitles$insert(    columnName,   newTitle);      },
    getColumnOvertitle  = function(columnName)                { return( private$jaspObject$colOvertitles      [[columnName]]);                },
    setColumnOvertitle  = function(columnName, newOvertitle)  {         private$jaspObject$colOvertitles$insert(columnName,   newOvertitle);  },
    getColumnFormat     = function(columnName)                { return( private$jaspObject$colFormats         [[columnName]]);                },
    setColumnFormat     = function(columnName, newFormat)     {         private$jaspObject$colFormats$insert(   columnName,   newFormat);     },
    getColumnCombine    = function(columnName)                { return( private$jaspObject$colCombines        [[columnName]]);                },
    setColumnCombine    = function(columnName, newCombine)    {         private$jaspObject$colCombines$insert(  columnName,   newCombine);    },
    getColumnType       = function(columnName)                { return( private$jaspObject$colTypes           [[columnName]]);                },
    setColumnType       = function(columnName, newType)       {         private$jaspObject$colTypes$insert(     columnName,   newType);       },
    getRowName          = function(rowIndex)                  { return( private$jaspObject$rowNames           [[rowIndex]]);                  },
    setRowName          = function(rowIndex, newName)         {         private$jaspObject$rowNames$insert(     rowIndex,     newName);       },
    getRowTitle         = function(rowName)                   { return( private$jaspObject$rowTitles          [[rowName]]);                   },
    setRowTitle         = function(rowName, newTitle)         {         private$jaspObject$rowTitles$insert(    rowName,      newTitle);      },
		setColNames         = function(val)                       {         private$jaspObject$setColNames(val)                                   },
		getColNames         = function()                          {         private$jaspObject$colNames                                           },
		setRowNames         = function(val)                       {         private$jaspObject$setRowNames(val)                                   },
		getRowNames         = function()                          {         private$jaspObject$rowNames                                           },
		nrow                = function()                          {         length(self$getRowNames())                                            },
		ncol                = function()                          {         length(self$getColNames())                                            }
	),
	active = list(
		transpose                = function(x) if (missing(x)) private$jaspObject$transpose                else private$jaspObject$transpose                <- x,
		transposeWithOvertitle   = function(x) if (missing(x)) private$jaspObject$transposeWithOvertitle   else private$jaspObject$transposeWithOvertitle   <- x,
		status                   = function(x) if (missing(x)) private$jaspObject$status                   else private$jaspObject$status                   <- x,
    showSpecifiedColumnsOnly = function(x) if (missing(x)) private$jaspObject$showSpecifiedColumnsOnly else private$jaspObject$showSpecifiedColumnsOnly <- x
	),
	private = list(
		setField = function(field, value) private$jaspObject[[field]] <- value,
		getField = function(field)        return(private$jaspObject[[field]])
	)
)

`[[<-.jaspTableR` <- function(x, field, value) {
	x$.__enclos_env__$private$setField(field, value)
	return(x)
}
`[[.jaspTableR`   <- function(x, field)
	x$.__enclos_env__$private$getField(field)
  
names.jaspTableR <- function(x) {
  x$getColNames()
}

`names<-.jaspTableR` <- function(x, value) {
  x$setColNames(value)
  x
}

dimnames.jaspTableR <- function(x) {
  list(x$getRowNames(), x$getColNames())
}

`dimnames<-.jaspTableR` <- function(x, value) {
  if (!is.list(value) || length(value) != 2L) 
    stop("invalid 'dimnames' given for jaspTable, expecting list of length two (rows, cols)")
  value[[1L]] <- as.character(value[[1L]])
  value[[2L]] <- as.character(value[[2L]])
  
  dimTable <- dim(x)
  if (dimTable[[1L]] != length(value[[1L]]) || dimTable[[2L]] != length(value[[2L]]))
   stop("invalid 'dimnames' given for jaspTable, length of names must match number of cols and rows")
  
  x$setRowNames(value[[1L]])
  x$setColNames(value[[2L]])
  x
}

dim.jaspTableR <- function(x) {
  c(x$nrow(), x$ncol())
}

jaspColumnR <- R6Class(
  classname = "jaspColumnR",
  inherit   = jaspOutputObjR,
  cloneable = FALSE,
  public    = list(
    initialize = function(columnName="", dependencies=NULL, scalarData=NULL, ordinalData=NULL, nominalData=NULL, nominalTextData=NULL, jaspObject = NULL) {
      if (!is.null(jaspObject)) {
        private$jaspObject <- jaspObject
        return()
      }

      if (columnName == "")
        stop("You MUST specify a name for the column you want to change the data of")

      if (jaspResultsCalledFromJasp()) {
        columnObj <- jaspResultsModule$create_cpp_jaspColumn(columnName)
      } else {
        checkForJaspResultsInit()
        columnObj <- create_cpp_jaspColumn(columnName)
      }

      if(!is.null(scalarData))      columnObj$setScale(scalarData)
      if(!is.null(ordinalData))     columnObj$setOrdinal(ordinalData)
      if(!is.null(nominalData))     columnObj$setNominal(nominalData)
      if(!is.null(nominalTextData)) columnObj$setNominalText(nominalTextData)

      if (!is.null(dependencies))
        columnObj$dependOnOptions(dependencies)

      private$jaspObject <- columnObj
      return()
    },
    setScale        = function(scalarData)  private$jaspObject$setScale(scalarData),
    setOrdinal      = function(ordinalData) private$jaspObject$setOrdinal(ordinalData),
    setNominal      = function(nominalData) private$jaspObject$setNominal(nominalData),
    setNominalText  = function(nominalData) private$jaspObject$setNominalText(nominalData)
  )
)
