import scala.util.control.Breaks.break

object Game extends App {

  // Init  the board
  def initBoard():Array[Array[Int]] ={
    val board = Array(Array(0,0,0,0,0,0),
      Array(0,0,0,0,0,0),
      Array(0,0,0,0,0,0),
      Array(0,0,0,0,0,0),
      Array(0,0,0,0,0,0),
      Array(0,0,0,0,0,0))
    board
  }

  def win(board:Array[Array[Int]], x:Int, y:Int): Boolean ={
    val player = 1
    var stopX = false
    var stopY = false
    var nbAlignes = 0
    var newX = x
    var newY = y
    var sensX = 1
    var sensY = 1


    // On va checker les verticaux
    while((nbAlignes <4) && !stopX){
      if (newX < 6 && newX >= 0) {
        if (board(newX)(newY) == player) {
          nbAlignes += 1
          newX += sensX
          println("x="+newY+" y="+newX+" nb="+nbAlignes)
        }
        else {
          if (sensX == 1) {
            newX = x - 1
            newY = y
            sensX = -1
          }
          else stopX = true
        }

      }
      else stopX  = true
    }


    // On ne part dans ce check que si on a pas déjà gagné
    if (stopX && nbAlignes < 4) {
      nbAlignes = 0
      newX = x
      newY = y


      // On va checker les horizontaux
      while ((nbAlignes < 4) && !stopY ) {
        if (newY < 6 && newY >= 0) {
          if (board(newX)(newY) == player) {
            nbAlignes += 1
            newY += sensY
            println("x=" + newY + " y=" + newX + " nb=" + nbAlignes+" (h)")
          }
          else {
            if (sensY == 1) {
              newY = y - 1
              newX = x
              sensY = -1
            }
            else stopY = true
          }

        }
        else stopY = true


      }
    }

    nbAlignes >= 4
  }



  def determineDrop(board:Array[Array[Int]], positionToDrop:Int): Any={
    if(board(5)(positionToDrop-1) == 0) 5
    else if(board(4)(positionToDrop-1) == 0) 4
    else if(board(3)(positionToDrop-1) == 0) 3
    else if(board(2)(positionToDrop-1) == 0) 2
    else if(board(1)(positionToDrop-1) == 0) 1
    else if(board(0)(positionToDrop-1) == 0) 0
    else null
  }

  def gameLoop(board:Array[Array[Int]]): Int ={
    var winner = 0
    var i,j = 0
    while (winner ==0) {

      for (i <- board.indices) {
        for (j <- board(0).indices) {
          print(board(i)(j) + " ")
        }
        println()
      }

      // Proposer un move au joueur
      var height: Any = null
      while (height == null) {
        println("Quelle action voulez vous faire ?")
        val col = scala.io.StdIn.readInt()
        println()
        height = determineDrop(board, col)
        if (height != null) {
          board(height.asInstanceOf[Int])(col-1) = 1
          println(win(board, height.asInstanceOf[Int], col-1))
        } else println("Mouvement impossible")
      }
    }
    winner
  }

  val board  = initBoard()
  val winner = gameLoop(board)


}