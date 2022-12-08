#' day5
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @import dplyr tidyr
day5 <- function(){
# DATA
warehouse <- tidyr::tribble( ~x1,   ~x2,     ~x3,  ~x4,    ~x5,  ~x6,    ~x7,   ~x8,  ~x9,
                      "[Q]",  NA,   "[P]", "[P]",    NA,    NA,    NA,    NA,   NA,
                      "[G]", "[V]", "[S]", "[Z]", "[F]",    NA,    NA,    NA,   NA,
                      "[W]", "[V]", "[F]", "[Z]", "[W]", "[Q]",    NA,    NA,   NA,
                      "[V]", "[T]", "[N]", "[J]", "[W]", "[B]", "[W]",    NA,   NA,
                      "[Z]", "[L]", "[V]", "[B]", "[C]", "[R]", "[N]", "[M]",   NA,
                      "[C]", "[W]", "[R]", "[H]", "[H]", "[P]", "[T]", "[M]", "[B]",
                      "[Q]", "[Q]", "[M]", "[Z]", "[Z]", "[N]", "[G]", "[G]", "[J]",
                      "[B]", "[R]", "[B]", "[C]", "[D]", "[H]", "[D]", "[C]", "[N]") %>%
              dplyr::mutate( sort = 1:nrow(.)) %>%
              dplyr::arrange(-sort) %>%
              dplyr::select(-sort) %>%
              as.list()
warehouse$x9 <- warehouse$x9[!is.na(warehouse$x9)]
warehouse$x8 <- warehouse$x8[!is.na(warehouse$x8)]
warehouse$x7 <- warehouse$x7[!is.na(warehouse$x7)]
warehouse$x6 <- warehouse$x6[!is.na(warehouse$x6)]
warehouse$x5 <- warehouse$x5[!is.na(warehouse$x5)]
warehouse$x2 <- warehouse$x2[!is.na(warehouse$x2)]




cmd_s <- utils::read.csv("R/day5cmd", header = F, sep = " " ) %>%
  dplyr::select(-V1, -V3, -V5) %>%
  dplyr::rename(crateNo = V2, fromStack = V4, toStack = V6)




extractCrates <- function( fromStack, toStack, crateNo){


  resList <- lapply(warehouse, tail, crateNo)[fromStack]
  resList <- as.character(unlist(resList))
  resList <- rlist::list.reverse(resList)
  noChars <- length(resList)
  resList <- c(as.character(unlist(warehouse[toStack])), resList)
  warehouse[[toStack]] <<- resList

  for (i in 1:noChars) {
    warehouse[[fromStack]] <<-
      warehouse[[fromStack]][-(length( warehouse[[fromStack]] ) -i+1)]
  }
}


test <- as.list(cmd_s)

mapply(extractCrates,
       test$fromStack,
       MoreArgs = list(toStack = test$toStack, crateNo = test$crateNo))

}
