\name{brute_force_knapsack}
\alias{brute_force_knapsack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
brute_force_knapsack(x, W)
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
    best_value <- 0
    best_combination <- NULL
    for (i in 1:(2^n)) {
        included <- as.logical(intToBits(i)[1:n])
        total_weight <- sum(x$w[included])
        total_value <- sum(x$v[included])
        if (total_weight <= W && total_value > best_value) {
            best_value <- total_value
            best_combination <- which(included)
        }
    }
    return(list(value = best_value, elements = best_combination))
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
