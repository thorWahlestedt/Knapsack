\name{knapsack_dynamic}
\alias{knapsack_dynamic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
knapsack_dynamic(x, W)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{W}{
%%     ~~Describe \code{W} here~~
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
##--	or standard data sets, see data().

## The function is currently defined as
function (x, W) 
{
    if (!is.data.frame(x) || !all(c("w", "v") \%in\% names(x)) || 
        any(x$w <= 0) || any(x$v <= 0) || W < 0) {
        stop("Invalid input.")
    }
    n <- nrow(x)
    dp <- matrix(0, n + 1, W + 1)
    for (i in 1:n) {
        for (w in 0:W) {
            if (x$w[i] > w) {
                dp[i + 1, w + 1] <- dp[i, w + 1]
            }
            else {
                dp[i + 1, w + 1] <- max(dp[i, w + 1], dp[i, w - 
                  x$w[i] + 1] + x$v[i])
            }
        }
    }
    best_value <- dp[n + 1, W + 1]
    w <- W
    elements <- c()
    for (i in n:1) {
        if (dp[i + 1, w + 1] != dp[i, w + 1]) {
            elements <- c(elements, i)
            w <- w - x$w[i]
        }
    }
    return(list(value = best_value, elements = elements))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
