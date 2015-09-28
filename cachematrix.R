## Put comments here that give an overall description of what your
## functions do
## 
## Primary objectives of the functions in this file is to compute inverse of a matrix
##  with processing optimization, which uses caching mechanism.  

## Write a short comment describing this function
##
## This function will create a cached version of the matrix to compute its inverse
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL	 
   }
   get <- function() x
   setinv <- function(solve) m <<- solve
   getinv <- function() m
   list(set = set, get = get, 
        setinv = setinv,
		getinv = getinv)
}


## Write a short comment describing this function
## 
## This function will use cached version of matrix created by makeCacheMatrix 
## and computes its inverse. The standard R function solve() is used to compute matrix inverse
## During computation, if the inverse of matrix is already computed once, and if the matrix has not
## been modified then the cached version is returned instead of computing matrix inverse again. 
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinv()
		if (!is.null(m)) {
		    message ("getting cached data")
			return (m)
		}
		data <- x$get()
		m <- solve(data,...)
		x$setinv(m)
		m
}
