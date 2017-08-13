

#class longitudinal data
Longitudinal <- setClass("Longitudinal", 
         slots = list(
             id = "numeric",
             visit = "integer",
             room = "factor",
             value = "numeric",
             timepoint = "numeric"
         ))

subject <- setClass("subject", 
         slots = list(
             id = "numeric"
         ),
         contains = "Longitudinal")

visit <- setClass("visit",
         slots = list(
             visit = "integer"
         ),
         contains = "Longitudinal")
room <- setClass("room",
         slots = list(
             room = "factor"
         ),
         contains = "visit")

setGeneric("make_LD",function(x){
    standardGeneric("make_LD")
})
setGeneric("subject",function(x,n){
    standardGeneric("subject")
})

setMethod("make_LD",
          c(x = "data.frame"),
          function(x){
              LD <- new("Longitudinal",
                        id = x$id,
                        visit = x$visit,
                        room = x$room,
                        value = x$value,
                        timepoint = x$timepoint) 
              
          })

setMethod("subject",
          c(x = "Longitudinal"),
          function(x, n){
              idx = c(x@id == n)
              new("subject",
                  id = n,
                  visit = x@visit[idx],
                  room = x@room[idx],
                  value = x@value[idx],
                  timepoint = x@timepoint[idx]
              )
              
          })



setMethod("print", c(x = "subject"),
          function(x){
              cat("ID: ",x@id)
              callNextMethod()
          })


setMethod("print", c(x = "visit"),
          function(x){
              cat("Visit: ", x@visit)
              callNextMethod()
          })














