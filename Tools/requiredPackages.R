args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0)
  stop("\nRequired arguments:\n",
       "\t(1) Path to jasp-desktop folder.\n",
       "\t(2) Optionally, boolean whether to install packages.\n",
       "\t(3) Optionally, boolean if the packages should be grouped per analysis.\n",
       "\t(4) Optionally, boolean if a list of the packages should be printed.\n",
       "\t(5) Optionally, boolean if a .csv should be written with package info per analysis")

jaspDir <- args[1]

install <- FALSE
if (length(args) > 1)
  install <- ifelse(tolower(args[2]) == "true", TRUE, FALSE)

perAnalysis <- FALSE
if (length(args) > 2)
  perAnalysis <- ifelse(tolower(args[3]) == "true", TRUE, FALSE)
  
printList <- FALSE
if (length(args) > 3)
  printList <- ifelse(tolower(args[4]) == "true", TRUE, FALSE)
  
writeCsv <- FALSE
if (length(args) > 4)
  writeCsv <- ifelse(tolower(args[5]) == "true", TRUE, FALSE)

if (!dir.exists(jaspDir))
  stop("Could not find directory ", jaspDir)

subDirs <- list.dirs(jaspDir, recursive=FALSE, full.names=FALSE)
if (!all(c("JASP-Engine", "JASP-R-Interface") %in% subDirs))
  stop("Could not locate JASP-Engine and JASP-R-Interface as subdirectories of ", jaspDir)

dirs <- c(file.path(jaspDir, "JASP-Engine"), file.path(jaspDir, "JASP-R-Interface"))
rFiles <- list.files(dirs, pattern="\\.[Rr]$", recursive=TRUE, full.names=TRUE)
if (length(rFiles) == 0)
  stop("Could not locate any R files in the JASP-Engine and JASP-R-Interface directories")

options("repos" = "https://cloud.r-project.org")
if (!"stringr" %in% installed.packages())
  install.packages("stringr") # needed to generate the required packages list more easily


findAllNamespaces <- function(rFile) {
  expr <- '([a-zA-Z0-9.]{2,}(?<![.]))(?:::|:::)[a-zA-Z0-9._]+' # valid namespace -> triple or double colons -> valid function name
  comment <- '#.*'

  content <- suppressWarnings(readLines(rFile))
  content <- gsub(comment, "", content) # remove comments
  matches <- stringr::str_match_all(content, expr)
  matches <- unlist(lapply(matches, function(match) match[, 2]))

  return(matches)
}

excludePackages <- function(pkgs, duplicates, base, jasp, sort=TRUE) {
  if (jasp)
    pkgs <- pkgs[!pkgs %in% c('JASPgraphs', "jaspResults", "jasptools")]

  if (duplicates)
    pkgs <- unique(pkgs)
  
  if (sort)
    pkgs <- sort(pkgs)

  if (base) {
    basePkgs <- installed.packages(priority="high")
    basePkgs <- basePkgs[basePkgs[, "Priority"] == "base", 1]
    pkgs <- pkgs[!pkgs %in% basePkgs]
  }
  
  if (length(pkgs) == 0)
    return(NULL)
  
  return(pkgs)
}

analyses <- vector("list", length(rFiles))
names(analyses) <- rFiles

for (rFile in rFiles) {
  matches <- findAllNamespaces(rFile)
  if (length(matches) == 0)
    next
  pkgs <- excludePackages(matches, base=FALSE, duplicates=TRUE, jasp=TRUE)
  if (!is.null(pkgs))
    analyses[[rFile]] <- pkgs
}

names(analyses) <- basename(names(analyses))

if (!perAnalysis) {
  reqPkgs <- unlist(analyses)
  
  # some packages are not detected correctly as a dependency (e.g., GPArotation is incorrectly marked as "Suggest' in psych)
  reqPkgs <- c(reqPkgs, "GPArotation","RcppArmadillo", "modules")

  reqPkgs <- excludePackages(reqPkgs, duplicates=TRUE, base=TRUE)
}

getDependencies <- function(pkgs) {
  deps <- tools::package_dependencies(pkgs, recursive=TRUE, which=c('Depends', 'Imports'))
  depPkgs <- unlist(deps)
  depPkgs <- sort(unique(depPkgs))
  depPkgs <- depPkgs[!depPkgs %in% pkgs]
  
  return(depPkgs)
}

printPkgs <- function(reqPkgs, depPkgs=NULL) {

  cat("\n\nRequired packages:\n")
  cat(paste0(reqPkgs, collapse=", "), "\n")
  
  if (!is.null(depPkgs)) {
    cat("\nDependencies of required packages [Imports, Depends]:\n")
    cat(paste0(depPkgs, collapse=", "), "\n")
  }
  
}

if (perAnalysis && printList) {
  
  for (i in seq_along(analyses)) {
    cat(paste("Analysis:", names(analyses)[i]))
    printPkgs(analyses[[i]])
    cat("-----\n")
  }
  
} else if (printList) {
  depPkgs <- getDependencies(reqPkgs)
  printPkgs(reqPkgs, depPkgs)
}

if (writeCsv && perAnalysis) {
  for (i in seq_along(analyses)) {
    system("pkglist.R -")
  }
} 

if (!perAnalysis) {
  if (install) {
    pkgsToinstall <- reqPkgs[!reqPkgs %in% installed.packages()]
    if (length(pkgsToinstall) > 0) {
      cat("Installing all missing packages...")
      for (pkg in pkgsToinstall)
        install.packages(pkg, dependencies = c("Depends", "Imports"), INSTALL_opts = c("--no-docs", "--no-html", "--no-multiarch"))
      cat("\nFinished iterating over the required packages\n")
    }
  } else {
    cat("\nInstall string:\n")
    strPkgs <- paste0("'", reqPkgs, "'")
    installString <- paste0("install.packages(c(", paste(strPkgs, collapse=", "), "), repos = 'https://cloud.r-project.org', dependencies = c('Depends', 'Imports'))")
    cat(installString)
  }
}
