\name{cts_prep}
\alias{cts_prep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Prepare CTS data for modelling with SimInf
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Reads in and prepares CTS data for modelling with the SimInf simulation framework.
}
\usage{
cts_prep(uid = NULL, pwd = NULL, startdate, enddate, model = c("SIR"), cphsizes_file = NULL, initdata_file = NULL, events_file = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{uid}{User ID for the EPIC server. If not specified in the function call, the user may be prompted to supply it if required.}
  \item{pwd}{Password for the EPIC server. If not specified in the function call, the user may be prompted to supply it if required.}
  \item{startdate}{Date for the start of the simulation model; specify in the format \code{'YYYY/MM/DD'}.}
  \item{enddate}{Date for the end of the simulation model; specify in the format \code{'YYYY/MM/DD'}.}
  \item{model}{Compartmental model type; currently only \code{'SIR'} is implemented.}
  \item{cphsizes_file}{Name of file containing CPH size data (i.e. numbers of animals per CPH on the start date of the simulation model) on the local file system, if available. Ignored if \code{inidata_file} is given. If both \code{cphsizes_file} and \code{initdata_file} are \code{NULL}, the required data will be extracted from the \code{cts201404} databased on the EPIC server.}
  \item{initdata_file}{Name of the file on the local file system containing the formatted initialisation data. If \code{NULL}, the \code{cphsizes_file} (if available) will be used to construct the initialisation data set.}
  \item{events_file}{Name of the file on the local file system containing the formatted events data. If \code{NULL}, the required data will be extracted from the EPIC server.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Initialisation and events data can be imported from files on the local file system, or extracted directly from the EPIC server. However, data extraction from the EPIC server takes considerably longer.

If data extraction from the EPIC server is necessary (in the case where either \code{initdata_file} and/or \code{events_file} are NULL), the user will be prompted to supply their user ID and password to access the EPIC server.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a model data object (list) containing the following components:
\item{initdata }{Initialisation data (number of animals at each location at the start of the simulation).}
\item{eventdata }{Observed event data to be used in the simulation model.}
\item{model }{Compartment model type.}
\item{startdate }{Start date for the simulation model.}
\item{enddate }{End date for the simulation model.}
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
Currently data preparation procedures are implemented for only the SIR model.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{cphsizes_estimate}}, \code{\link{cts_events_extract}}
}
\examples{
## -- EXAMPLE NOT RUN --

## Extract and prepare CTS data from the EPIC server
# cts_prep(startdate = '2012/01/01', enddate = '2012/03/31', model = 'SIR')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
