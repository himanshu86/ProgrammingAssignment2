
## makeCacheMatrix function

makeCacheMatrix<-function(x=matrix())
{
    i <- NULL
    set <- function(y) 
    {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve<-function(y)
{
    i <- y$getinv()
    if(!is.null(i)) 
    {
        message("getting cached data")
        return(i)
    }
    data <- y$get()
    i <- solve(data)
    y$setinv(i)
    i
}
        ## Return a matrix that is the inverse of 'x'

