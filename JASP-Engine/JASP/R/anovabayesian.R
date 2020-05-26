#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


anovabayes <- function(data, formula, inNullModel=NULL, priorFixed=0.5, priorRandom=1, bfOrder="nullModelTop", bfType="BF10", 
                       effects=FALSE, effectsType="allModels", posthocVars=NULL, posthocNullCorr=TRUE, sampleMode="auto", numSamples=10000,
                       descriptives=FALSE, descriVarOnYAxis="", descriVarAsLines="", descriVarAsPlots="", descriShowCI=FALSE, descriCIWidth=95, 
                       plotWidth=480, plotHeight=320) {
  require(jaspResults)
  options <- as.list(match.call())
  
  options$dep <- .dependentsFromFormula(formula)
  options$factors <- .independentsFromFormula(formula, type="fixed")
  options$random <- .independentsFromFormula(formula, type="random")
  options$modelTerms <- .termsFromFormula(formula, controlColumn=list(inNullModel=inNullModel, type="boolean"))
  
  jaspResults <- initJaspResults()
  AnovaBayesian(jaspResults=jaspResults, dataset=options$data, options=options)
  jaspResults$summary()
  return(invisible(jaspResults$asResults()))
}

AnovaBayesian <- function(jaspResults, dataset, options, ...) {

  .BANOVArunAnalysis(jaspResults, dataset, options, "ANOVA")

}
