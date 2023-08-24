\name{ucpay}
\alias{ucpay}
\title{Query the UC wage data}
\description{
  Query by person's name or title for a given year and campus or all.
}
\usage{
ucpay(name = NA, title = NA, location = "Davis", year = 2022, maxNum = NA, nrows = 1000, nd = now(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{a character vector with one or two elements in the order last name and first name}
  \item{title}{the employment title }
  \item{location}{which campus or generally location. These are values that come from the names of \code{CampusNames}}
  \item{year}{the year for which salaries to query}
  \item{maxNum}{the maximum number of records to return. This is used to stop the paging through the
  results.}
  \item{nrows}{the number of rows in each page of results}
  \item{nd}{should not be used. Gives the current time}
  \item{\dots}{additional arguments passed in each call to \code{\link[RCurl]{getURLContent}}}
}
\value{
A data frame with 10 columns
\item{Index}{record number}
\item{Year}{the year, e.g., 2022}
\item{Location}{the name of the campus or location generally}
\item{FirstName, LastName}{first and last name of person}
\item{Title}{employment title of the person}
\item{GrossPay,RegularPay,OvertimePay,OtherPay}{amounts for the different categories of pay}  
}
\references{
 \url{https://ucannualwage.ucop.edu/wage/}
}
\author{
Duncan Temple Lang
}


%\seealso{}
\examples{
    z = ucpay(title = "PROF OF CLIN-SFT-VM")
    z = ucpay("Temple Lang")
    z = ucpay(maxNum = 3000)
}
\keyword{IO}

