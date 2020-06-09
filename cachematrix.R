## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix is a function that creates
# a list. That list sets the value of the square
# matrix, then gets the value of the matrix,
# followed by setting the value of the inverse
# matrix and getting the value of the inverse matrix.

# cacheSolve will decide whether the matrix has been
# inverted before(without change), if so it will
# cache the stored value of the inverse matrix. 
# If this is a new calculation then it will do the 
# calculation. This matrix will not solve a matrix, 
# without the MakeCacheMatrix as it's arguement in
# the function. 
  
## Write a short comment describing this function
# makeCacheMatrix function is made up of 4 functions with 
# x as a matrix. The set function sets y to x and mat to null.
# The get function prints the value of x. The setinverse
# function will solve the inverse of the matrix once it has been
# passed to CacheSolve and will set to mat to the inverse of the matrix
# The getinverse will print mat in the cacheSolve function
# we make a list of set and setinverse, which will be used to solve
# the inverse matrix in the cacheSolve function. The other part of 
# the list is the get and getinverse, which will be used in the 
# cacheSolve to cache the inverse of the matrix.
# We assign a value to matrix A (square non singular matrix) which 
# is put into the makeCacheMatrix and assigned to x.

makeCacheMatrix <- function(x = matrix()) {
      mat <- NULL
      set <- function(y) {
            x <<- y
            mat <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) mat <<- solve
      getinverse <- function() mat
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

x <- makeCacheMatrix(A) 
## Write a short comment describing this function
# cacheSolve 
# mat is set to a subset of x (from MakeCacheMatrix)
# if it isn't null i.e. it has been calculated before, 
# it will print a message printing "getting cache data",
# and will return the result without calculating it. If
# the matrix hasn't been calculated then it will solve 
# the inverse of the matrix.
# Note that the Cachesolve will only cache the inverse
# matrix if the matrix hasn't changed since the last operation
# of cacheSolve. If, for example, cacheSolve of the matrix A, 
# followed by CacheSolve of matrix B, then return to 
# cacheSolve matrix A, it will not cache the inverse of matrix A.
# It will only cache matrix A if the previous cacheSolve was 
# performed on matrix A

cacheSolve <- function(x, ...) {
      mat <- x$getinverse()
      if(!is.null(mat)) {
            message("getting cached data")
            return(mat)
      }
      data <- x$get()
      mat <- solve(data, ...)
      x$setinverse(mat)
      mat
}
