#######################################################################
## Function for plotICS
##
## Plot measures of location by cluster size
## Quantitative  variable: mean, median, SD, variance, IQR, range
##
## Categorical variable: barplot
##
## Function should operate on vectors (of individual observations) or table
#######################################################################

#' Test of Marginal Proportion for Clustered Data
#'
#' Function to visualize informative cluster size. Plots within-cluster summary statistic from
#' quantitative variables against the size of each cluster. For categorical variables, a barplot of
#' category proportions for quantiles of cluster size is produced.
#'
#' @param x  vector of data values. Alternatively a two-dimensional table or matrix.
#' @param id a vector which identifies the clusters, with length equal to length of \code{x}; ignored if \code{x} is a matrix or table.
#' @param FUN the name of the function that produces the desired intra-cluster summary statistic.
#' @param breaks a single number giving the number of desired quantiles for the barplot of categorical variables with >2 categories.
#' @param xlab a label for the x axis, defaults to "cluster size".
#' @param ylab a label for the y axis, defaults to a description of \code{FUN} of \code{x}.
#' @param legend a logical indicating whether a legend should be included in a barplot.
#' @param ... further arguments to be passed to or from methods.
#'
#' @details If \code{x} is a matrix or table and \code{x} has exactly two columns, the first column should contain the cluster
#'  sizes and the second column the respective intra-cluster summary statistic (e.g., mean, variance) that will be plotted
#'  against cluster size.
#'
#'  If \code{x} has more than two columns, the first column is assumed to contain the cluster size
#'  and the subsequent columns the counts of intra-cluster observations belonging to the different categorical variable levels.
#'  If there are exactly two categorical levels (e.g., \code{x} has exactly three columns), a scatterplot of the proportion of
#'  intracluster observations belonging to the first category will be plotted against the cluster size. If the number of
#'  categories is > 2, a barplot of category proportions against quantiles of cluster size is produced.
#'
#'  Standard graphical parameters can be passed to \code{icsPlot} through the \code{...} argument.
#'
#' @examples
#' data(screen8)
#' ## VECTOR INPUT
#' ## plot average math score by cluster size
#' icsPlot(x = screen8$math, id = screen8$sch.id, pch = 20)
#'
#' ## plot proportion of females by cluster size
#' icsPlot(screen8$gender, screen8$sch.id, pch = 20, main = "Female proportion by cluster size")
#'
#' ## barchart of activity proportion by quartile of cluster size
#' icsPlot(x = screen8$activity, id = screen8$sch.id)
#'
#' ## TABLE INPUT
#' ## Plot intra-cluster variance of math score by cluster size
#' cl.size <- as.numeric(table(screen8$sch.id))
#' tab1 <- cbind(cl.size, aggregate(screen8$math, list(screen8$sch.id), var)[,2])
#' colnames(tab1) <- c("cl.size", "variance")
#' icsPlot(x = tab1, pch = 17, main = "math score variance by cluster size")
#'
#' ## barchart of activity proportion across five quantiles of cluster size
#' tab2 <- cbind(cl.size, table(screen8$sch.id, screen8$activity))
#' icsPlot(tab2, breaks = 5)
#'
#' @export

icsPlot <- function(x, id, FUN = c("mean", "median", "var", "sd", "range", "IQR", "prop"),
                    breaks, xlab = NULL, ylab = NULL, legend = c(TRUE, FALSE), ...) {
  ## check validity (length of vectors is same, other checks)
  if (!missing(breaks) && (!is.finite(breaks) || breaks < 2 ))
    stop("'breaks' must be a single number > 2")
  xLAB <- ifelse(!is.null(xlab), paste(xlab), paste("cluster size"))

  ##  TABLE INPUT
  if (is.table(x) || is.matrix(x)) { ## first column cluster size, second column desired summary
    ## NUMERIC
    if (ncol(x)==2) {
      yLAB <- ifelse(!is.null(ylab), paste(ylab), paste(colnames(x)[2]))
      graphics::plot(x[,1], x[,2], xlab = xLAB, ylab = yLAB, ...)
    }
    ## FACTOR
    else {## first column cluster size, addition columns are counts from respective clusters across categorical levels
      if (!(all(x>=0)))
        stop("elements of x must be nonnegative when plotting categorical variable")
      tmp.tab <- x[,-1]
      ## if two levels, do scatter plot of proportion of first value
      if (ncol(tmp.tab) == 2) {
        px <- prop.table(tmp.tab, margin=1)[,1]
        ylab.tmp <- paste("proportion", paste(colnames(tmp.tab)[1]))
        yLAB <- ifelse(!is.null(ylab), paste(ylab), ylab.tmp)
        graphics::plot(x[,1], px, xlab = xLAB, ylab = yLAB, ...)
      }
      else {
        ## if > two levels, group into cluster size quartile (or specified number of breaks) and do bar chart
        if (missing(breaks))
          breaks <- 4
        cl.size.cat <- cut(x[,1], breaks = stats::quantile(x[,1], probs=seq(0,1,by=1/breaks), na.rm=TRUE), include.lowest = TRUE,
                           right = FALSE)
        tmp <- data.frame(x[,-1], cl.size.cat)
        count <- stats::aggregate(list(tmp[,1:(ncol(tmp)-1)]), list(tmp[,ncol(tmp)]), sum)
        tmptab <- apply(count[,2:ncol(count)], 1, function(x) x/sum(x))
        colnames(tmptab) <- count[,1]
        yLAB <- ifelse(!is.null(ylab), paste(ylab), "proportion")
        legend.in <- legend
        graphics::barplot(as.table(tmptab), legend = legend.in, xlab = xLAB, ylab = yLAB, ...)
      }
    }
  }
  ## VECTOR INPUT
  else { ## each value in x and id are an observation
    if ((l <- length(x)) != length(id))
      stop("'x' and 'id' must have the same length")
    FUNin <- match.arg(FUN)
    cl.size <- as.numeric(table(id))
    DNAME <- deparse(substitute(x))

    if (is.factor(x)) {
      if(!missing(FUN) & FUNin!="prop")
        stop(paste("'FUN = ", paste0(paste(FUN),"'"), "invalid for factors"))
      nx <- levels(x)
      ## if two levels, do scatter plot of proportion of first value
      if (length(nx) == 2) {
        px <- prop.table(table(id, x), margin=1)[,1]
        ylab.tmp <- paste("proportion", paste(DNAME, " = ", paste(nx[1])))
        yLAB <- ifelse(!is.null(ylab), paste(ylab), ylab.tmp)
        graphics::plot(cl.size, px, xlab = xLAB, ylab = yLAB, ...)
      }
      else {
      ## if > two levels, group into cluster size quartile (or specified number of breaks) and do bar chart
      if (missing(breaks))
        breaks <- 4
      cl.size.exp <- rep(cl.size, cl.size)
      cl.size.cat <- cut(cl.size.exp, breaks = stats::quantile(cl.size.exp, probs=seq(0,1,by=1/breaks), na.rm=TRUE), include.lowest = TRUE,
                         right = FALSE)
      counts <- table(x, cl.size.cat)

      yLAB <- ifelse(!is.null(ylab), paste(ylab), "proportion")
      legend.in <- legend
      graphics::barplot(prop.table(counts, margin=2), legend = legend.in, xlab = xLAB, ylab = yLAB, ...)
      }
    }
    else {
      FUN.tmp <- ifelse(FUNin=="range", "range", "not range")
      if (FUN.tmp == "range") {
        tmp <- tapply(x, list(id), FUNin)
      }
      sumval <- switch(FUN.tmp, "not range" = tapply(x, list(id), FUNin),
                       "range" = apply(matrix(unlist(tmp), ncol=2, byrow=T), 1, diff))
      ylab.tmp <- paste(FUNin, DNAME)
      yLAB <- ifelse(!is.null(ylab), paste(ylab), ylab.tmp)
      graphics::plot(cl.size, sumval, xlab = xLAB, ylab = yLAB, ...)
    }
  }
}
