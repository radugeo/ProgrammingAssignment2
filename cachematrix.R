## MATRIX CALCULATORS WITH CACHING FUNCTION ---------------------------------

## Function makeCacheMatrix ---------------------
## Function will create a list of functions that calculate matrix inversion

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## Function cacheSolve ------------------------ 
## Function will calculate the inverse of a matrix
## Calculation will check if cached inverse value exists

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
fn <- makeCacheMatrix(a)
cacheSolve(fn)
