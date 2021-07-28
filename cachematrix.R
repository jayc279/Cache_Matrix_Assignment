"
  Function `makeCacheMatrix` uses Function `inv` from package matlib
    > install.packages('matlib')
    > library(matlib)

  Put comments here that give an overall description of what your
  functions do
  To test if a MATRIX can be inverted provide the following args to
  makeCacheMatrix and check inversion with cacheSolve
  
  > source('cachematrix.R')
  > x <- matrix(c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3)
  > y<- makeCacheMatrix(x)
  > cacheSolve(y)
  
"

" 
  Using matlib::inv generates an ERROR
  
  Error in Inverse(X, tol = sqrt(.Machine$double.eps), ...) : 
  X is numerically singular
  
  Instead use matlib::Ginv
  
  Source: https://cran.r-project.org/web/packages/matlib/vignettes/ginv.html
"

"
  This Function `makeCacheMatrix` creates a special 'matrix' object
  that can cache its inverse.
  Assumption here is that 'x' is a SQUARE MATRIX

  # install matlib package
  # load library matlib
  > library(matlib) 
"

makeCacheMatrix <- function(x = matrix()) {

  # print (x) # check input
  ## Check if 'x' is a SQUARE matrix - nrow and ncol MUST be equal
  if ( nrow(x) == ncol(x) ) {
    # Continue if TRUE
    # Initialize m - NULL - largely used to the lists with ZERO length
    m <- NULL
    
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    setinv <- function(Ginv) m <<- Ginv
    getinv <- function() m
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
  } else {
    return(message("Cannot invert -- 
                   nrow and ncol of input matrix are not equal"))
  }

}

"
  This function `cacheSolve` computes the inverse of the special
  'matrix' returned by `makeCacheMatrix` above. If the inverse has
  already been calculated (and the matrix has not changed), then
  `cacheSolve` should retrieve the inverse from the cache.
"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  # print(m) # check what is `m` if NULL something went wrong
  
  ## 'm' is not NULL, return cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  # If m is NULL, get x and invert Matrix, cache and return inverted matrix
  data <- x$get()
  # print(class(data)) # check if `data` is a matrix
                        # list contents if needed to verify
  m <- Ginv(data, ...)
  # print(m)  # print m and check if inverted
  x$setinv(m)

  # Return m
  m

}
