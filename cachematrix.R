##Programming Assignment 2
## R-Programming course
## DataScience Specialization

## Assignment on Lexical Scoping
## Two functions to cache inversion of matrix

## Function 1 - generates a list in place of a input matrix, 
## storing the inverse of the matrix if there is no inverse saved already.
## input x = matrix()
## output x = list (set, get, setinv, getinv)

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  setmatrix <- function(y){ 
    x <<- y
    invx <<- NULL
  }
  getmatrix <- function() x
  setinv <- function(solve) invx <<- solve
  getinv <- function() invx
  list(set = setmatrix, get = getmatrix, setinv = setinv, getinv = getinv)
}


## Function 2 - computes inverse of the input matrix that has already been created
## in the above function. Gets the matrix from the list, checks for cached inverse
## if NULL, computes and stores the inverse in the getinv


cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getinv()
  if(!is.null(invx)){
    message("getting cached inverse matrix")
    return(invx)
  }
  origmatrix <- x$get()
  invx <- solve(origmatrix)
  x$setinv(invx)
  invx
}
