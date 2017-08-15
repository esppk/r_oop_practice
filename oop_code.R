

#class longitudinal data
Longitudinal <- setClass("Longitudinal", 
         slots = list(
             id = "numeric",
             visit = "numeric",
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
             visit = "numeric"
         ),
         contains = "subject")
room <- setClass("room",
         slots = list(
             room = "factor"
         ),
         contains = "visit")
setClass("summary",
         slots = list(
             id = "numeric",
             df = "data.frame"
         )) 
 
setGeneric("make_LD",function(x){
    standardGeneric("make_LD")
})
setGeneric("subject",function(x,n){
    standardGeneric("subject")
})
setGeneric("visit",function(x,n){
    standardGeneric("visit")
})
setGeneric("room",function(x,n){
    standardGeneric("room")
})
setGeneric("summary", function(x){
    standardGeneric("summary")
})

setMethod("make_LD",
          c(x = "data.frame"),
          function(x){
              LD <- new("Longitudinal",
                        id = x$id,
                        visit = x$visit,
                        room = as.factor(x$room),
                        value = x$value,
                        timepoint = x$timepoint) 
              
          })

setMethod("subject",
          c(x = "Longitudinal"),
          function(x, n){
              idx = c(x@id == n)
              if(any(idx)){
                  new("subject",
                      id = n,
                      visit = x@visit[idx],
                      room = x@room[idx],
                      value = x@value[idx],
                      timepoint = x@timepoint[idx] ) 
              }else{return(NULL)}
              
            
              
          })

setMethod("visit",
          c(x = "Longitudinal"),
          function(x, n){
              idx = c(x@visit == n)
              new("visit",
                  visit = n,
                  id = x@id,
                  room = x@room[idx],
                  value = x@value[idx],
                  timepoint = x@timepoint[idx]
              )
              
          })

setMethod("room",
          c(x = "Longitudinal"),
          function(x, n){
              n <- factor(n, level = c("bedroom", "den","dining room", 
                                              "family  room" ,"hall", "kitchen",    
                                              "living room", "office", "study room",  
                                              "tv room" ))
              idx = c(x@room == n)
              new("room",
                  room = n,
                  visit = x@visit,
                  id = x@id,
                  value = x@value[idx],
                  timepoint = x@timepoint[idx]
              )
          })



setMethod("print", c(x = "subject"),
          function(x){
              
              cat("ID: ",x@id)
          })


setMethod("print", c(x = "visit"),
          function(x){
              callNextMethod()
              cat("\n")
              cat("Visit: ", x@visit)
          })

setMethod("print", c(x = "room"),
          function(x){
              callNextMethod()
              cat("\n")
              cat("Room: ", as.character(x@room))
          })
setMethod("print",c(x = "Longitudinal"),
          function(x){
              cat("Longitudinal dataset with", length(levels(ld@room)))
              cat(" subjects")
          })

setMethod("print",c(x = "summary"),
          function(x){
              cat("ID: ",x@id)
              cat("\n")
              x@df
          })

setMethod("summary",c(x = "subject"),
          function(x){
              require(tidyr)
              require(dplyr)
              df <- tibble(visit = x@visit, room = x@room,
                           value = x@value)
            
              df <- df %>% group_by(visit,room) %>% 
                  summarise(avg = mean(value)) %>% 
                  spread(room, avg)
              class(df) <- "data.frame"
              new("summary",
                  id = x@id,
                  df = df)
          })

setMethod("summary", c(x = "room"),
          function(x){
              s <- summary.default(x@value)
              name <- names(s)
              attributes(s) <- NULL
              l <- list()
              s <- as.data.frame(lapply(seq_along(s), function(x){l[x] = s[x]}))
              names(s) <- name
              new("summary",
                  id = x@id,
                  df = s)
          })












