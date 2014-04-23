## cacheSolve and makeCacheMatrix are functions, they let us to create a special object that stores a matrix
## and cache큦 its inverse


## makeCacheMatrix is a function, it creates a special "matrix". it return a list with 4 list items
## (set, get, setInverse and getInverse).


makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function (y){                            ## set the value of the matrix
              x <<- y
              m <<- NULL
        }
        get <- function() x                             ## get the value of the matrix
        setInverse <- function(Inverse) m <<- Inverse   ##set the value ot fhe Inverse
        getInverse <- function() m                      ##get the value of the Inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
      }


## cacheSolve is a function, it calculates the inverse of the special "matrix" created but first check if
## it has been calculated.

cacheSolve <- function(x,...){
        m <- x$getInverse()   ## check if the inverse has been calculated from makeCacheMatrix
        if(!is.null(m)){
            message("getting cache data")
            return(m)         ## return the cache큦 inverser
        }
       
        data <- x$get()       ## if there큦 no a cache큦 inverse
        m <- solve(data,...)  ## it calculate the inverese of x
        x$setInverse(m)       ## save the result back to x큦 cache 
        m                     ## return the result
      }