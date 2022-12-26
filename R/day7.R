
#' day7
#'
#' @return {directory list}
#' @export
#' @importFrom magrittr %>%
#' @import dplyr tidyr stringr readr

day7 <- function() {
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
               value = stringr::str_sub(cmdStr, start = 6, end = nchar(cmdStr)))
      }

      if (stringr::str_detect(cmdStr, '[[:digit:]]')) {
        # res <- list(case = "file", value = str_sub(cmdStr, start = 1, end = 6 ))


        res <-
          list(case = "file",
               value = as.numeric(readr::parse_number(cmdStr, '[[:digit:]]' )),
               name = str_sub(cmdStr,
                              start = str_locate(cmdStr , "[[:alpha:]]" )[1],
                              end = nchar(cmdStr))
               )
      }

      if (stringr::str_sub(cmdStr, start = 1L, end = 3) == 'dir') {
        res <-
          list(case = "dir",
               value = stringr::str_sub(cmdStr, start = 5, end = -1L))
      }

      if (stringr::str_detect(cmdStr, '\\$ cd \\.\\.')) {
        res <- list(case = "upp", value = "")
      }

      return(res)
    }

    cmdLogg <- read.csv('data/day7rawData', header = F)

    dirStructure <- tidyr::tibble( dir = character(), sDir = character())
    fileStructure <- tidyr::tibble ( fName = character(), dir = character(), size = double())


    dir <- subDir <- "root"
    row <- 1

    repeat {

      res <- parser(cmdLogg$V1[row])
      # print(paste(res$case, "-", res$value))
      if ( res$case == "cd" ){
              if (res$value != ".."){
                dir <- res$value
              }
      }

      if ( res$case == "dir"){
        subDir <- res$value
        dirStructure <- rbind( dirStructure, tibble(dir = dir, sDir = subDir))
      }

      if (res$case == "file") {
        fileStructure <- rbind(fileStructure, tibble(
          fName = res$name, dir = dir, size = res$value))
      }


      if (row == nrow(cmdLogg)) {
        break
      } else {
        row <- row + 1
      }
    }
    return( list( dirStructure = dirStructure, fileStructure = fileStructure)  )
  }

  # ------ MAIN

  res <-findStructure()

  fileStructure <- tidyr::as_tibble(res$fileStructure )
  dirStructure <- tidyr::as_tibble( res$dirStructure )

  fileStructure <-
    fileStructure %>%
    dplyr::group_by(dir) %>%
    dplyr::summarize(totSize = sum(size))

  allDirs <- tibble(all = unique(c(as.character(dirStructure$dir),
                      as.character(dirStructure$sDir))))

  res <-
    left_join(dirStructure, fileStructure, by = c("sDir" = "dir")) %>%
    group_by(dir) %>%
    summarize (totSize = sum(totSize, na.rm = T)) %>%
    filter(totSize < 100000)
  return(res)
}
day7()
