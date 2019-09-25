## MATRIX CALCULATORS WITH CACHING FUNCTION ---------------------------------

## Function makeCacheMatrix ---------------------
## Function will create a list of functions that calculate matrix inversion

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x # get returns x = input matrix
    setinverse <- function(solve) m <<- solve # store inverse matrix in m
    getinverse <- function() m # get m, can be NULL or the inverse matrix
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## Function cacheSolve ------------------------ 
## Function will calculate the inverse of a matrix
## Calculation will check if cached inverse value exists

cacheSolve <- function(x, ...) {
    m <- x$getinverse() # check getinverse here
    if(!is.null(m)) {   # if it's not NULL then use cache
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get() # get data from x = matrix to be calculated
    m <- solve(data, ...) # solve here the inverse matrix
    x$setinverse(m) # use set function to save inverse calculation
    m
}

a <- matrix(c(3,2,3,4), nrow = 2, ncol = 2) # input data matrix
fn <- makeCacheMatrix(a) # construct list object
cacheSolve(fn) # first time it's called, it should not return message
