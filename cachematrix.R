## Programming Assignment 2: Lexical Scoping

## This work is based on code from the R programming course  "Example: Caching the Mean of a Vector"

## As matrix inversion is a costly computation, it is advantageous to caching the inverse of a matrix rather than compute it 
## again and again. 
## 
## The following pair of functions cache the inverse of a matrix.  
## It is assumed that the matrix supplied is always invertible.


## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse. This function actually
## returns a list containing a function to: 
## 1-set the value of the matrix
## 2-get the value of the matrix
## 3-set the value of the inverse
## 4-get the value of the inverse

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
## Usage:   mat <- makeCacheMatrix()
##          mat$set(matrix(c(4,3,1,2),2,2))


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cacheSolve function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinv(m)
        m
}

## Usage: cacheSolve(mat)