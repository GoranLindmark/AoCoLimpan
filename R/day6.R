library(stringr)

indata <- "bvwbjplbgvbhsrlpgdmjqwftvncz"


allCharsUnique <- function(str){
 str <- unlist(strsplit(str, ""))
 if ( !str[1] %in% str[2:4] )

 1  2:4
 2  1,3:4
 3  1:2, 4
 4  1:3

 }

window <- function(indata, anchor){

  str_sub(indata, anchor-3, anchor)
}

anchor <- 4
for (anchor in 4:str_length(indata)){
  print(window(indata, anchor))
}

str <- "ajoi"
