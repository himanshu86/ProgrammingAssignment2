#The below functions fetch the inverse from cache if the result has already been computed
#previously else it computes and displays the result

#This function returns a list of functions for setting and getting data for the matrix

makeCacheMatrix<-function(x=matrix())
{
    i <- NULL
    set <- function(y) {
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


#This function uses the functions of "makeCacheMatrix" to get the inverse of a matrix
#If the inverse is being computed for the first time, it computes and displays the result
#Else it gets the result from cache and displays

cacheSolve<-function(y)
{
    i <- y$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- y$get()
    i <- solve(data)
    y$setinv(i)
    i
}