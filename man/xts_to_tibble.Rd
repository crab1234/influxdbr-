\name{xts_to_tibble}
\alias{xts_to_tibble}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xts_to_tibble(x, timestamp_format)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{timestamp_format}{
%%     ~~Describe \code{timestamp_format} here~~
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
function (x, timestamp_format) 
{
    performance <- FALSE
    timer <- function(x, txt) {
        message(paste(Sys.time(), txt))
        x
    }
    div <- get_precision_divisor(timestamp_format)
    result_na <- tibble::tibble(statement_id = NA, series_names = NA, 
        series_tags = NA, series_values = NA, series_partial = NA)
    while (!("results" \%in\% names(x))) {
        x <- purrr::flatten(x)
    }
    x <- purrr::flatten(x)
    list_of_result <- purrr::map(x, .f = function(series_ele) {
        result <- result_na
        if (!is.null(series_ele$statement_id)) {
            statement_id <- series_ele$statement_id
        }
        else {
            statement_id <- NA_integer_
        }
        if (!is.null(series_ele$series)) {
            series_names <- purrr::map(series_ele$series, "name") \%>\% 
                unlist() \%>\% if (is.null(.)) 
                NA
            else .
            series_tags <- purrr::map(series_ele$series, "tags") \%>\% 
                purrr::map(tibble::as_tibble)
            series_columns <- purrr::map(series_ele$series, "columns") \%>\% 
                purrr::map(unlist)
            series_values <- purrr::map(series_ele$series, "values") \%>\% 
                if (performance) 
                  timer(., "transpose data")
                else . \%>\% purrr::map(~purrr::transpose(.)) \%>\% 
                if (performance) 
                  timer(., "convert influxdb NULL to NA")
                else . \%>\% purrr::map(~purrr::map(., ~purrr::map(., 
                ~. \%||\% NA))) \%>\% if (performance) 
                timer(., "unlist data")
            else . \%>\% purrr::map(~purrr::map(., base::unlist)) \%>\% 
                if (performance) 
                  timer(., "unify numerics")
                else . \%>\% purrr::map(~purrr::map_if(., is.integer, 
                as.double)) \%>\% if (performance) 
                timer(., "setting column names")
            else . \%>\% purrr::map2(., .y = series_columns, ~purrr::set_names(., 
                nm = .y)) \%>\% if (performance) 
                timer(., "set POSIX-based time index")
            else . \%>\% purrr::map(~purrr::map_at(., .at = "time", 
                ~as.POSIXct(./div, origin = "1970-1-1", tz = "GMT")) \%>\% 
                tibble::as_tibble(., validate = FALSE))
            series_partial <- ifelse(is.null(series_ele$partial), 
                FALSE, TRUE)
            result <- tibble::tibble(statement_id, series_names, 
                series_tags, series_values, series_partial)
        }
        else {
            if (!is.null(series_ele$error)) {
                stop(series_ele$error, call. = FALSE)
            }
            warning("no series returned")
        }
        return(result)
    })
    return(list_of_result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
