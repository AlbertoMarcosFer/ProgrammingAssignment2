## The code below computes the inverse of the matrix x.

## The following function (makeCacheMatrix) requires a single argument: a matrix x.
## makeCacheMatrix clears the memory of the console by setting m to NULL both in the 
## makeCacheMatrix Environment (m <- NULL) and in the Global Environment (m <<- NULL).
## Four functions (set(), get(), setInverse() and getInverse()) that will be used in the
## CacheSolve function are created .

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The following function (cacheSolve) also requires a single argument: an object
## X <- makeCacheMatrix(x= matrix())
## cacheSolve first checks whether an inverse matrix calculation has already been stored
## (if a value for X$getInverse() already exists). If that is the case (i.e., if(!is.null(m)))
## it will return the message "getting cached data" and the previously stored value.
## If there is no stored value, it will make use of the get(), solve() and setInverse()
## arguments to compute it.

cacheSolve <- function(X, ...) {
        m <- X$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- X$get()
        m <- solve(data, ...)
        X$setInverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}


## An example of the way it works would be:
> X <- makeCacheMatrix(x= matrix(c(1, 2, 3, 4), nrow=2, ncol=2))
> cacheSolve(X)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

## If the same input is passed again, we would get the "getting cached data" message:
> cacheSolve(X)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

## Thank you for your time :)
