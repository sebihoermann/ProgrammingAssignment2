## R Programming Assignment 2:
## https://class.coursera.org/rprog-012/human_grading/view/courses/973493/assessments/3/submissions
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function:
makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  set <- function(u) {
    x <<- u
    cm <<- NULL
  }
  get <- function() x
  set_inverse_matrix <- function(inverse) cm <<- inverse
  get_inverse_matrix <- function() cm
  list(set = set, get = get,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix = get_inverse_matrix)
}
## The following function calculates the mean of the special "vector" created with the above function. 
## However, it first checks to see if the mean has already been calculated.

cacheSolve <- function(x, ...) {
  ## Create a reversed Matrix of matrix x:
  cm <- x$get_inverse_matrix()
  if(!is.null(cm)) {
    message("Getting cached data for reversed Matrix...")
    return(cm)
  }
  df <- x$get()
  cm <- solve(df, ...)
  x$set_inverse_matrix(cm)
  cm
}