## The combination of these two functions produces the inverse of a given matrix,
## taking advantage of caching and of scoping rules of the R language.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) Inv <<- solve
        getInv <- function() Inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cacheSolve retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getInv()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInv(Inv)
        Inv        
}

## example
## A is a square invertible matrix
## A <- matrix(c(2, 4, -3, 1 ), nrow = 2, ncol = 2, byrow = TRUE)
## 
## A
##      [,1] [,2]
## [1,]    2    4
## [2,]   -3    1
## 
## 
## mi <- makeCacheMatrix(A)
## 
## mi$get()
##      [,1] [,2]
## [1,]    2    4
## [2,]   -3    1
## 
## cacheSolve(mi)       #  <--- run1: the function computes the inverse matrix and put it in cache 
##       
##            [,1]       [,2]
## [1,] 0.07142857 -0.2857143
## [2,] 0.21428571  0.1428571
## 
## cacheSolve(mi)        #  <--- run2: running it once more, instead of compute the inverse matrix again,
##                                 it just gets the inverse matrix from cache, where it was placed 
##                                 in the previous step
## getting cached data   #  <---
##            [,1]       [,2]
## [1,] 0.07142857 -0.2857143
## [2,] 0.21428571  0.1428571
