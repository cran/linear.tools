% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear.tools.R
\name{get_contrast}
\alias{get_contrast}
\title{get contrast of categorical variables in a model}
\usage{
get_contrast(model, data = NULL, PRINT = TRUE, return_method = FALSE,
  delete.minus.var = TRUE)
}
\arguments{
\item{model}{a model, either \code{lm} or \code{glm}.}

\item{data}{dataframe, to provide new data to evaluate the model. If NULL (default), then we use the default data in the model.}

\item{PRINT}{a boolean, whether to print messages. Default is TRUE.}

\item{return_method}{a boolean, whether to return the method of contrast, rather than the contrast itself. Default is FALSE.}

\item{delete.minus.var}{a boolean. whether to delete x2 in  y ~ x1 - x2. Default is TRUE.}
}
\value{
contrasts of the categorical vars in the model, or the contrast method if \code{return_method} is TRUE.
}
\description{
get contrast of categorical variables in a model
}
\details{
When R put categorical vars in the linear model, R will transform them into set of 'contrast' using
certain contrast encoding schedule. See example code and the reference link below for details.
}
\examples{

get_contrast(lm(price ~ carat + I(carat^2) + cut:carat +
                  color,ggplot2::diamonds))
get_contrast(lm(price ~ carat + I(carat^2) + cut:carat +
                  color,ggplot2::diamonds),return_method = TRUE)

# dirty formulas: all categorical vars are with minus sign
# no categorical vars, thus no contast
get_contrast(lm(price ~ carat + I(carat^2) ,ggplot2::diamonds))

model_dirty = lm(price ~ carat + I(carat^2) - cut:carat - color,
                 ggplot2::diamonds)
get_contrast(model = model_dirty )

diamond_lm3 = lm(price~ I(cut) + depth,ggplot2::diamonds) # a GLM
get_contrast(model = diamond_lm3 )

}
\references{
\url{http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm}
}

