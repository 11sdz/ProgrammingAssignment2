## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#*  makeCacheMatrix function returns a list of special object that is a list that
#*contain 4 methods (get,get_inverse,set & set_inverse)

makeCacheMatrix <- function(x = matrix()) {
  cach_invmtx<-NULL
  set<-function(y){
    ##changing the matrix x to a different matrix will also change the inverse matrix
    if(!identical(x,y)){
      x<<-y
      cach_invmtx<<-NULL
    }
  }
  get <-function() x
  set_inverse<-function(inverted) cach_invmtx<<-inverted
  get_inverse<-function() cach_invmtx  
  list(set=set , get = get,
       set_inverse= set_inverse,
       get_inverse= get_inverse)
  
}


## Write a short comment describing this function
#*  cacheSolve(x) input: our special matrix object 
#*  output inverse matrix
#*  at the first call to the method the function will calculate the inverse matrix
#*  at the second call and so on with the same object without changing the original matrix
#*  the method will return the inverse matrix from cache without another calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverted<-x$get_inverse()
        if(!is.null(inverted)){
          message("getting cached matrix")
          return(inverted)
        }
        inverted<-solve(x$get())
        x$set_inverse(inverted)
        inverted
}