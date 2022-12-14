day7
library(tidyverse)
library(stringr)
library(readr)




day7 <- function(){




  findStructure <- function() {
    parser <- function(cmdStr) {
      # browser()

      if (stringr::str_detect(cmdStr, '\\$ cd')) {
        res <-
          list(case = "cd",
               value = str_sub(cmdStr, start = 6, end = -1))
      }

      if (stringr::str_detect(cmdStr, '\\$ ls')) {
        res <- list(case = "ls", value = "")
      }

      if (stringr::str_detect(cmdStr, '\\$ dir')) {
        res <-
          list(case = "ls",
               value = str_sub(cmdStr, start = 6, end = nchar(cmdStr)))
      }

      if (stringr::str_detect(cmdStr, '[[:digit:]]')) {
        # res <- list(case = "file", value = str_sub(cmdStr, start = 1, end = 6 ))
        res <- list(case = "file", value = as.numeric(parse_number(cmdStr)))
      }

      if (stringr::str_sub(cmdStr, start = 1L, end = 3) == 'dir') {
        res <-
          list(case = "dir",
               value = str_sub(cmdStr, start = 5, end = -1L))
      }

      if (stringr::str_detect(cmdStr, '\\$ cd \\.\\.')) {
        res <- list(case = "upp", value = "")
      }

      return(res)
    }

    cmdLogg <- read.csv("data-raw/day7rawDataSmall", header = F)

    dirrrs <- character()
    subdirr <- character()
    size <- double()
    tree <- data.frame(dirrrs, subdirr, size)

    row <- 1  # row in command log
    dirName <- NA
    subdir <- NA
    ls <- F

    outRow <- 1
    dirName <- subdir <- "root"

    repeat {


      res <- parser(cmdLogg$V1[row])
      print(paste(res$case, "-", res$value))

      if (res$case == "cd") {

        subdir <- dirName
        dirName <= as.character(res$value)
        ls <- F
      }

      if (res$case == "dir") {

      }

      if (res$case == "ls") {
        ls <- T
      }
      if (res$case == "file") {
        browser()
        tibble(dirrrs = dirname,
                   subdirr = subdir,
                   size = res$value)
        tree <- data.frame(dirrrs = dirname,
                   subdirr = subdir,
                   size = res$value)
        tree <- rbind(tree, data.frame(dirrrs = dirname,
                                               subdirr = subdir,
                                               size = res$value))
        outRow <- outRow + 1
      }




      if (row == nrow(cmdLogg)) {
        return(tree)
        break
      } else {
        row <- row + 1
      }
    }

  }
  findStructure()
  return(tree)
}
day7()

