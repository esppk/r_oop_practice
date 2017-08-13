
#class longitudinal data
Longitudinal <- setClass("Longitudinal", 
         slots = list(
             id = "numeric",
             visit = "integer",
             room = "factor",
             value = "numeric",
             timepoint = "numeric"
         ))

setClass("subject", 
         slots = list(
             id = "numeric"
         ),
         contains = "Longitudinal")

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
              Longitudinal(
                  id = x@id[idx],
                  visit = x@visit[idx],
                  room = x@room[idx],
                  value = x@value[idx],
                  timepoint = x@timepoint[idx]
              )
              
          })



setMethod("print", c(x = "Longitudinal"),
          function(x){
              cat()
          })

















