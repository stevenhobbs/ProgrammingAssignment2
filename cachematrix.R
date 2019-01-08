## The two functions below will find the inverse of a matrix as quickly as possible. 
## Novel matrices passed to the functions will be calculated (slow) and cached (for future referrence). 
## Repeat matrices will be obtained from cache (fast), rather than from calculation (slow).


## makeCacheMatrix creates a list containing 4 functions and 2 objects 
## "i" is a local variable defaulted as "NULL", but which becomes the inverse 
##  of the matrix supplied to makeCacheMatrix when cacheSolve is run on an object of makeCacheMatrix.
##  x is the matrix passed to makeCacheMatrix, or that is defined in the parent environment using set()
##  set() resets the value of x (the original matrix) to a new user supplied matrix, and resets i to NULL
##  get() retrieves the matrix supplied as input to makeCacheMatrix
##  setinverse() changes the value of i to "InvertedMatrix" which is calculated in cacheSolve()
##  getinverse() returns the value of i that is cached.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x<<- y # The parent environment x object (from x=matrix()) is assigned to y
        i <<-NULL
    }
    get <- function() x
    setinverse <- function(InvertedMatrix) i <<-InvertedMatrix
    getinverse <- function() i
    list(set = set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}


## cacheSolve evaluates a makeCacheMatrix object and returns the inverse of the 
## matrix passed to makeCacheMatrix, which is obtained from cache if it exists, or
## it is calculated using solve() and then stored in cache.

cacheSolve <- function(x, ...) {   #x is a makeCacheMatrix object.  This x is not the original matrix
    i <- x$getinverse() # i will be NULL, if matrix is novel. i will be the inverse, if the matrix is a repeat.
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
    ## Return a matrix that is the inverse of 'x' (the original matrix passed to makeCacheMatrix)
}
