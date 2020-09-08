## File cachematrix.R
## The file contains code to implement two functions: makeCacheMatrix and cacheSolve.
## Usage: The file must be loaded into an R workspace with the source function.
## Example: source(cachematrix.R)
## The example assumes that the R file is saved in the working directory of the active session of F

## Function makeCacheMatrix(x= matrix())
## The function converts its parameter in a matrix if it is not already a matrix. The function returns a list object
## with for elements of the list:
## set: Is a function that receives the matrix from the call of the function makeCacheMatrix and store its value in the 
## variable x of its environment. It also sets the null value to the varialbe m which indicates that the inverse of 
## the square matrix has not been calculated.
## get: Is a function that returns the value of the matrix which was originally store by the set function.
## setinverse: if a function that receives the inverse of the matrix stored in x.
## getinverse: is a function that returns the value of the variable m which is the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Function: cacheSolve(x,...)
## The function receives a list object created by the makeCacheMatrix function and returns the inverse of the matrix. If the matrix
## is the same one as the last matrix recieved when the function was called, then, it gets its inversed from the value m from
## the makeCacheMatrix function environment. If the matrix is not the same as the last one received (or if it is the first call) then it calculates its inverse
## it calculates the inverse of the matrix using the solve function. After calculating the inverse it stores the result 
## in the variable m of the makeCacheMatrix environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached value")
    return (m)
  }
  matrix <- x$get()
  m <- solve(matrix,...)
  x$setinverse(m)
  m
}
