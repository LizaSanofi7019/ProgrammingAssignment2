
## this function, itself containing 4 functions set, get, setinverse, and getinverse, allows to store the matrix/inverse values and replace them
## 'i' denotes the inverse of the function and is first set to null. 
## the set function allows to replace the old matrix value with a new one 
## the get function allows to retrieve the matrix from cache 
## getinverse allows to retrieve the inverse value from cache 
## setinverse sets a new inverse value into cache

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## the cacheSolve() function takes the matrix as the argument and decides whether to compute an inverse from scratch using the built in solve()
## function or to get an inverse from cache data (which takes less processing time - in this particular case the difference isn't noticeable becuase we are computing 
## the inverse for just one matrix but a similar approach can make a difference if working with large datasets)

## if the i variable isn't null the function will print out a message that the inverse value is retrieved from cache 
## if the i varialbe is null then the inverse is computed with the solve() function and set to the setinverse() item in the list 

cacheSolve <- function(x, ...) {
  i <- mymatrix$getinverse()
  if(!is.null(i)) {
    message("getting inverse value from cache")
    return(i)
  }
  data <- mymatrix$get()
  i <- solve(data, ...)
  mymatrix$setinverse(i)
  i
}

############
## RUN TESTS 
m <- makeCacheMatrix(matrix(c(3,5,2,8), nrow=2, ncol=2)) ##create a matrix 
m$get()
     [,1] [,2]
[1,]    3    2
[2,]    5    8    ## check that the matrix has appeared in the list 

m$getinverse()
NULL              ## the inverse hasn't been calculated yet to it's null 

cacheSolve(m)
           [,1]       [,2]
[1,]  0.5714286 -0.1428571
[2,] -0.3571429  0.2142857  ## apply the cacheSolve function 

m$getinverse()
           [,1]       [,2]
[1,]  0.5714286 -0.1428571
[2,] -0.3571429  0.2142857   ## the inverse appears on the list 

cacheSolve(m)
getting inverse value from cache
           [,1]       [,2]
[1,]  0.5714286 -0.1428571
[2,] -0.3571429  0.2142857  ## the inverse value wasn't calculated again but obtained from cache instead

m$set(matrix(c(3,7,8,8), nrow=2, ncol=2))
m$get()
     [,1] [,2]
[1,]    3    8
[2,]    7    8    ## we replace the matrix in the list by another one 

cacheSolve(m)
         [,1]     [,2]
[1,] -0.25000  0.25000
[2,]  0.21875 -0.09375  ## solve it 

m$getinverse()
         [,1]     [,2]
[1,] -0.25000  0.25000
[2,]  0.21875 -0.09375  ## new inverse is not in cache 

