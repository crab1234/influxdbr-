\name{nb_ets}
\alias{nb_ets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nb_ets(data, freq, predlenth, force_positive)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{freq}{
%%     ~~Describe \code{freq} here~~
}
  \item{predlenth}{
%%     ~~Describe \code{predlenth} here~~
}
  \item{force_positive}{
%%     ~~Describe \code{force_positive} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, freq, predlenth, force_positive) 
{
    for (j in 1:length(data$results.series_values)) {
        series_tags <- as.data.frame(data$results.series_tags[j])
        series_names <- paste(data$results.series_names[j], "pred", 
            sep = "_")
        temp <- as.data.frame(data$results.series_values[j])
        temp <- temp[complete.cases(temp), ]
        y = ts(temp$y, frequency = freq)
        res_y = stlm(y, s.window = freq, robust = FALSE, method = "ets", 
            etsmodel = "ZZN")
        pred_y = forecast(res_y, method = "ets", etsmodel = "ZZN", 
            forecastfunction = NULL, h = predlenth, level = 80, 
            fan = FALSE, lambda = res_y$lambda, biasadj = NULL, 
            xreg = NULL, newxreg = NULL)
        history <- history(data = temp, pred_y = pred_y, freq = freq, 
            width = 2)
        future <- future(data = temp, pred_y = pred_y, freq = freq, 
            pred_length = predlenth)
        output = rbind(history, future)
        if (force_positive) {
            output[output < 0] <- 0
        }
        output <- cbind(output, series_tags)
        influx_write(con = con, db = db, x = output, time_col = "time", 
            tag_cols = colnames(output)[-c(1:4)], measurement = series_names)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
