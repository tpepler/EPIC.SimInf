\name{cphsizes_estimate}
\alias{cphsizes_estimate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Estimate CPH sizes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Estimates number of cattle per CPH ("CPH size") from CTS movement data extracted from the EPIC server.
}
\usage{
cphsizes_estimate(uid, pwd, modeldata, outfile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{uid}{User ID for the EPIC server.}
  \item{pwd}{Password for the EPIC server.}
  \item{modeldata}{A list containing the model data. Only the components labelled \code{model} and \code{startdate} are used in this function; for more details on how to specify the modeldata object, see the example at the end of this help file.}
  \item{outfile}{Name of file on local system where the output should be written to. If none is supplied, a default descriptive file name will be assigned.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
For the specified date, the number of cattle per CPH is estimated by counting all animals moved onto a CPH before the date, without any record indicating that they have been moved off of the CPH before the date. Oly CTS movement records where \code{is_valid_history = 't'} are included in this calculation.

CPH size estimates are returned for all CPHs (in England, Wales and Scotland) for which the estimates are non-zero.

As the data extraction and calculation takes a considerable amount of computational time (~40 minutes), the output can be saved on file for future use.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a data frame with the location IDs (field \code{location_id} from the \code{movements} table in the CTS database), CPH numbers, and number of cattle at each CPH on the specified date.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Theo Pepler
}
\note{
%%  ~~further notes~~
The saved output file can be used in the \code{\link{cts_prep}} function without needing to re-estimate the CPH sizes each time.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{cts_prep}}
}
\examples{
## -- EXAMPLE NOT RUN --

## Initialise model object
# modeldata1 <- vector('list', 5)
# names(modeldata1) <- c('initdata', 'eventdata', 'model', 'startdate', 'enddate')
# modeldata1$model <- 'SIR'
# modeldata1$startdate <- '2012/01/01'

## Estimate CPH sizes
# cphsizes(uid = 'MyUserID', pwd = 'MyPassword', modeldata = modeldata1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
