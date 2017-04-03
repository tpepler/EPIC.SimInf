\name{cts_events_extract}
\alias{cts_events_extract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Extract CTS events data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Extracts CTS movement records from the EPIC server, and organises it in the format required for the specified SimInf model.
}
\usage{
cts_events_extract(uid, pwd, modeldata, outfile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{uid}{User ID for the EPIC server.}
  \item{pwd}{Password for the EPIC server.}
  \item{modeldata}{A list containing the model data. The components labelled \code{model}, \code{startdate} and \code{enddate} are used in this function; for more details on how to specify the modeldata object, see the example at the end of this help file.}
  \item{outfile}{Name of file on local system where the output should be written to. If none is supplied, a default descriptive file name will be assigned.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Only CTS movement records for which \code{is_trans = 'f'} and \code{is_valid_history = 't'} are included in the events data set. Records are returned for all of England, Wales and Scotland.

As the data extraction and formatting takes a considerable amount of computational time (~20 minutes for the data from a single year), the output can be saved on file for future use.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a data frame with events data (entries, exits, internal transfers, external transfers) as required by the SimInf simulation framework.

The saved output file can be used in the \code{\link{cts_prep}} function without needing to re-extract and format the CTS events data each time.
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
Currently only formatting for the SIR model is implemented.
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
# modeldata1$enddate <- '2012/03/31'

## Extract CTS events data
# cts_events_extract(uid = 'MyUserID', pwd = 'MyPassword', modeldata = modeldata1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
