# data manipulation
## input: 1) test result, a pdf file; 2) test result excel



library(pdftools)
library(dplyr)


# deal with the last table: 1) delete since the "Continue on page" part; 2) delete last 5 elements
last.table <- function(table){
  # input: the last table
  # output: table after manipulation
  
  if (sum(grepl("Continued on next page",table)) == 0){
    table[1:(length(table)-5)]
  } else {
    table[1:(grep('Continued on next page',table)-1)]
    
  }
}

make.table <- function(temp){
  # input: content of each page
  # output: a list of table in this page
  title.idx <- grep("List of Metabolites:",temp)
  
  (start.idx <- title.idx+4)
  (end.idx <- c(title.idx[-1]-4,length(temp)))
  
  title <- trimws(gsub("List of Metabolites:","",temp[title.idx]))
  
  table.raw <- lapply(seq_along(start.idx), function(i) trimws(temp[start.idx[i]:end.idx[i]]))
  
  if (length(table.raw) == 1) {
    table.out <- list(last.table(last(table.raw)))
  } else {
    table.out <- c(table.raw[1:(length(table.raw)-1)],list(last.table(last(table.raw))))
  }
  
  names(table.out) <- title
  return(table.out)
}

lifespain_range <- vector("list", length(lifespan.list)-1) # no lipids right now
lb.total <- ub.total <- vector("list",8)

for (ppl in 1:8) {
  # part A: extract range
  file_name <- paste0("../Original Report/",ppl,"A.pdf")
  
  range <- pdf_text(file_name)[5:9]
  
  ## A.1 extract table from page 5 - 8, output: table.total
  table.total <- list()
  for (j in 1:(length(range)-1)) {
    temp <- strsplit(range, "\n")[[j]] # a list contain all the table in a page
    table.total <- c(table.total,make.table(temp))
  }
  
  ## A.2 split table into three column: item, result, range
  ### save the lower bound into a list and a data frame - lb, upper cound similarly
  lb.list <- lapply(table.total, function(temp){
    
    split.raw <- data.frame(do.call(rbind,lapply(strsplit(temp,"  "), function(x){
      if(length(grep("omitted",x))==0) { # if the range equals "omitted", delete
        idx <- nchar(x) > 0
        x[idx]
      }
    })))
    
    split <- split.raw %>% transmute(item = X1, 
                                     bound = as.numeric(lapply(strsplit(X3," - "),function(x) x[1]))) 
    bound <- t(split$bound)
    colnames(bound) <- t(split$item)
    return(bound)
  })
  lb <- do.call(cbind,lb.list)
  lb.total[[ppl]] <- lb
  
  ub.list <- lapply(table.total, function(temp){
    
    split.raw <- data.frame(do.call(rbind,lapply(strsplit(temp,"  "), function(x){
      if(length(grep("omitted",x))==0) { # if the range equals "omitted", delete
        idx <- nchar(x) > 0
        x[idx]
      }
    })))
    
    split <- split.raw %>% transmute(item = X1, 
                                     bound = as.numeric(lapply(strsplit(X3," - "),function(x) x[2]))) 
    bound <- t(split$bound)
    colnames(bound) <- t(split$item)
    return(bound)
  })
  ub <- do.call(cbind,ub.list)
  ub.total[[ppl]] <- ub
  
  # Part B: extract person, then fit the bound of each lab test
  ## for each person, variable XX_inrange would be added into each category element, indicating whether the variable is within the range
  idx.person <- (person.idx == as.character(ppl))
  
  person <- lapply(lifespan.list[-12],function(x) x[idx.person,]) # each person, without the lipids vars
  person$phase <- phase[idx.person]
  person$outcome <- outcome[idx.person]
  
  ## initialize a list contain XX_inrange variable 
  person_inrange <- vector("list",(11)) #length = cate[1:11]
  
  lapply(seq_along(person)[1:11], function(cate){ # for each category (length = cate[1:11])
    
    temp.person <- temp.person.range <- person[[cate]]
    
    # each test
    for (i in 1:ncol(temp.person)) {
      var.names <- toupper(names(temp.person)[i])
      var.idx <- toupper(colnames(lb)) == var.names
      
      in_range <- ifelse(temp.person[,i] < lb[,var.idx],0,ifelse(temp.person[,i] > ub[,var.idx],2,1)) # 0 for lower than lower bound, 2 for greater than upper bound, 

      #in_range <- factor(in_range,levels = c(0,1,2),labels = c("L","M","H"))
      
      temp.person.range <- cbind(temp.person.range,in_range)
      colnames(temp.person.range)[ncol(temp.person.range)]<- paste0(var.names,"_inrange")
      
    }
    temp.person.range$people <- ppl
    temp.person.range$phase <- person$phase
    temp.person.range$outcome <- person$outcome
    person_inrange[[cate]] <<- temp.person.range
  })
  
  
  
  lapply(seq_along(person_inrange), function(odr){
    lifespain_range[[odr]] <<- rbind(lifespain_range[[odr]],person_inrange[[odr]])
  })
  names(lifespain_range) <- names(person)[1:11]#length = cate[1:11]
}

# check missing value in the range data
lapply(lb.total, function(x) sum(is.na(x)))
lapply(ub.total, function(x) sum(is.na(x)))

# receive personal data
lifespain_range

lifespain_range_cate <- lapply(lifespain_range, function(x) x %>% select(ends_with("_inrange")))

lapply(lifespain_range_cate, is.na) # subject 2 do not have range for propionic, under the category of organic acids
