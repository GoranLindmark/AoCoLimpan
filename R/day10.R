#' day10
#'
#' @return
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
day10 <- function(){

  disp <- as.data.frame(read.csv("data/day10rawDataSmall", sep = " ", header = F))
  names(disp) <- c("cmd", "xValue")

  disp <-
    disp %>%
    dplyr::mutate(rep = ifelse(cmd == "addx", 2, 1)) %>%
    dplyr::slice(rep(1:n(), rep)) %>%
    dplyr::select(-rep) %>%
    dplyr::mutate(cycle = 1:n()) %>%
    dplyr::mutate(X1 = 0) %>%
    dplyr::select(cycle, cmd, xValue, X1)


  X1 <- 1
  row <- 1

 repeat {


    if( disp$cmd[row] == "addx"){
      disp$X1[row] <- X1
      X1 <- disp$X1[row+1] <- disp$X1[row] + disp$xValue[row]

      row <- row + 1
    }

    if( disp$cmd[row] == "noop"){
      disp$X1[row] <- X1
    }


    if ( row == nrow(disp) ){
      break
    }
    row = row + 1

 }
  disp <-
    disp %>%
    dplyr::mutate(signal = X1*cycle)

  print(disp[100, signal])

}
