## The combination of functions computes the inverse of an array and stores the result
## in memory, for optimization purposes in R, for the scenarios where the data set is reused.

## The makeCacheMatrix function creates a list containing the functions whose purpose is:
## to set and obtain the value of the matrix that it receives as a parameter, 
## and to establish and obtain the value of the corresponding inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinver <- function(inverse) inver <<- inverse
        getinver <- function() inver
                list(set = set, get = get, setinver = setinver, getinver = getinver)
}                

## The function cacheSolve returns the inverse of the array that 
## receives the parameter, evaluating if it has already calculated previously, 
## in that case it omits the calculation, otherwise, it calculates the inverse
## and sets the value in the cache through the function setinver().

cacheSolve <- function(x, ...) {
        inver <- x$getinver()
                if(!is.null(inver)) {
                        message("getting cached data")
                        return (inver)
                }
                data <- x$get()
                inver <- solve(data)
                x$setinver(inver)
                inver
}


## Example matrix:                
##> z <- matrix(1:4, 2, 2)
##> m = makeCacheMatrix(z)
##> m$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(m)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
                
##...After second run:                
##> cacheSolve(m)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5             
                
                
