#' day9
#'
#' @export
#' @import tidyr
day9 <- function(){

  #  needed data


  cmdLogg <- as.data.frame(read.csv('data/day9rawDataSmall', header = F, sep = " " ))
  names(cmdLogg) <- c("dir", "steps")

  s <- Tail <- Head <- data.frame(x = 5, y = 1)

  gameBoard <- tidyr::tibble(x1 = rep("*", 5), x2 = rep("*", 5),
                             x3 = rep("*", 5), x4 = rep("*", 5),
                             x5 = rep("*", 5), x6 = rep("*", 5))

  scoreboard <- gameBoard

  gameBoard[Head$x, Head$y ] <- "H"



  # needed functions
  hasContact <- function(Head, Tail){

    if (( abs(Head$x - Tail$x ) <= 1 ) && ( abs(Head$y - Tail$y ) <= 1 ) )  {

      return(T)
    } else {
      if (( abs(Head$x - Tail$x ) == 1 ) && ( abs(Head$y - Tail$y ) == 1 ) ) {
        return(T)
      } else {
         return(F)
      }


    }
  }
  moveTail <- function(Head, Tail, dir) {
    # browser()
    if ((abs(Head$x - Tail$x) == 2))  {
      if (Head$x - Tail$x < 0) {
        Tail$x <- Tail$x  - 1

        if (abs (Head$y - Tail$y) == 1) {
          if (Head$y - Tail$y  == 1) {
            Tail$y <- Tail$y + 1
          } else {
            Tail$y <- Tail$y - 1
          }
        }
      } else {
        Tail$x <- Tail$x + 1
      }
    }
    if (abs(Head$y - Tail$y) == 2) {
      if (Head$y - Tail$y < 0) {
        Tail$y <- Tail$y - 1

        if (abs (Head$x - Tail$x) == 1) {
          if (Head$x - Tail$x  == 1) {
            Tail$x <- Tail$x + 1
          } else {
            Tail$x <- Tail$x - 1
          }
        }

      } else {
        Tail$y <- Tail$y + 1
      }
    }

    return(Tail)
  }
  moveHead <- function(row, cmdLogg, Head){

    if(cmdLogg$dir[row] == "U"){
      Head$x <- Head$x - 1
    }

    if(cmdLogg$dir[row] == "D"){
      Head$x <- Head$x + 1
    }
    if(cmdLogg$dir[row] == "L"){
      Head$y <- Head$y - 1
    }

    if(cmdLogg$dir[row] == "R"){
      Head$y <- Head$y + 1
    }

    return(Head)
  }

  # Main program

  print(gameBoard)
  row <- 1

  repeat {

    for ( i in 1: cmdLogg$steps[row] ){


      if ( all(Head == Tail) ){
        gameBoard[Head$x, Head$y] <- "T"
        Head <- moveHead(row, cmdLogg, Head)
        gameBoard[Head$x, Head$y] <- "H"
      } else {
        gameBoard[Head$x, Head$y] <- "*"
        Head <- moveHead(row, cmdLogg, Head)
        gameBoard[Head$x, Head$y] <- "H"
      }

      scoreboard[Tail$x, Tail$y] <- "o"


      if ( !hasContact(Head, Tail) ){

        gameBoard[Tail$x, Tail$y] <- "*"
        Tail <- moveTail(Head, Tail)
        gameBoard[Tail$x, Tail$y] <- "T"
      }

      print(gameBoard)
    }

    if (row == nrow(cmdLogg)){
      break
    }
    row <- row + 1
  }

  print(scoreboard)

  }



