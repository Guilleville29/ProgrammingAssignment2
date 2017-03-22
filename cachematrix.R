## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

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


                
> m$get()
     [,1] [,2]
[1,]    1    2
[2,]    2    1                
                
                
    
> cacheSolve(m)
getting cached data
           [,1]       [,2]
[1,] -0.3333333  0.6666667
[2,]  0.6666667 -0.3333333               
                
                
