## Carlos H. Trevino - 2015-08-22
## These functions implement a cache to store the inverse of matrix so it
## doesn't gets recalculated each time to you need it

## This function creates and stores the matrix passed as parameter and it's
## inverse in two variables accesible through a list of 4 functions set, get,
## setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
    ## This variable stores de inverse of the matrix
    I <- NULL;
    ## The set variable take a matrix as parameter and stores it in x
    set <- function(y) {
        ## If the matrix changes delete (set to null) the inverse cached
        ## But just in case check if the matrix really changed
        if(!identical(x,y)){
            I <<- NULL;
            x <<- y;            
        }
    }
    ## The get variable just returns x
    get <- function() x;
    ## The setinverse function is similar to get, taking a matrix as parameter
    ## but stores it to I instead of x
    setinverse <- function(inverse) I <<- inverse;
    ## The getinverse just returns the variable I which stores de inverse of x
    getinverse <- function() I;
    ## this returns the list with the 4 functions defined to access x and I
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates de inverse of the matrix in x, but before doing the
## calculation it check if it's already calculated so it doesn't waste time
## doing the calculation again
## The variable x is not the matrix to be inverted but the list of functions
## returned from the function above (makeCacheMatrix)

cacheSolve <- function(x, ...) {
    ## First we check if the cache already contains a matrix (inverse)
    ## The set function in makeCachMatrix already makes sure the cache
    ## is null if the matrix is changed (set invoked)
    I <- x$getinverse()
    if(!is.null(I)){
        message("getting cached data");
        return(I);
    }
    ## If cache is null then calculate the inverse
    ## The function solve calculates de inverse of matrix
    data <- x$get();
    I <- solve(data);
    x$setinverse(I);
    I
}
