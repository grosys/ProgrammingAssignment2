## These two functions combine to cache the inv of a matrix.
## The first creates a list of functions that inits the process
## The second uses the list to check for and return the cached inverse
##     If there's no cached inverse matrix it computes the inverse.

## This is the init step: creates list object to identify if an 
##     inverse is cached and the cached matrix itself.

makeCacheMatrix <- function(oldMat = matrix()) {
    invMat <- NULL                ## don't have inv yet.
    set <- function(y) {          
        oldMat <<- y              ## creates fcn lex scope calls oldMat 
        invMat <<- NULL
    }
    get <- function() oldMat      
    setInv <- function(solve) invMat <<- solve  ## called fcn drops invMat
                                                ## in this environment
    getInv <- function() invMat   ## hopefully gives cached inverse
    list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInv()
  if(!is.null(invMat)) {
      message("getting cached data")
      return(invMat)
    }
  data <- x$get()
  invMat <- solve(data, ...)      ## here's the money step Inv compute
  x$setInv(invMat)                ## Use of fcn from Cmatrix list object
  invMat
}
