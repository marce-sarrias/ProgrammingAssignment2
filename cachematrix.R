## This is Marcela's Programming Assignment 2 for the R Programming Course

## this function creates a 'special' matrix object that can catch its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y ## <<- operator assigns a value to an object in an environment that is different from the current one
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse =  getInverse)
}

## this function returns the a matrix that is the inverse of x (see function above)
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...) ## this the function that solves the equation
        x$setInverse(inv)
        inv
}

## Let's test the function

## test <- makeCacheMatrix(matrix(5:8, 2, 2))

## test$get()
##      [,1] [,2]
## [1,]    5    7
## [2,]    6    8  

## test$getInverse()
## NULL

## cacheSolve(test)
##      [,1] [,2]
## [1,]   -4  3.5
## [2,]    3 -2.5

## it works! it calculates the inverse matrix of test