\name{printThread}
\alias{printThread}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
printThread function
}
\description{
Prints the posted messages.
}
\usage{
printThread(thread, index = 1:nrow(thread), by = 1, format = function(post) {
    email <- sprintf("[%s]", post$Email)
    datetime <- strftime(as.POSIXct(post$DateTime, origin = "1970-01-01 09:00:00"), "%Y/%m/%d(%a) %H:%M:%S")
    header <- paste(post$Number, post$Name, email, datetime)
    message <- paste("--", header, "--", post$Message, sep = "\n")
    return(message)
})
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{thread}{
	A thread object (data frame).
	}
	\item{index}{
	Indexes of the messages
	}
	\item{by}{
	How many messages for each output.
	}
	\item{format}{
	A function to format messages.
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
kos59125
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
