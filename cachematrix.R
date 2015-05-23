## Just as the Instructions of the Assignment,
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix a list containing a function to
# 1. set the value of the matrix(set)
# 2. get the value of the matrix(get)
# 3. set the value of inverse of the matrix(setinverse)
# 4. get the value of inverse of the matrix(getinverse)
## A simple mimic from the given instructions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set  <- function(y){
                x <<- y
                inv <<- NULL 
        }
        get  <- function() x
        setinverse  <- function(inverse) inv  <<- inverse
        getinverse  <- function() inv
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## cachesolve, the following function calculates the inverse of the special "vector" created with the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## As you can see above, these lines are just look similar to the codes provided.
## I hope this works fine, Good Luck to All!