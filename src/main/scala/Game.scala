
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
    false
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

      for (i <- 0 until board.length) {
        for (j <- 0 until board(0).length) {
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
        if (height != null)
          board(height.asInstanceOf[Int])(col-1) = 1
        else println("Mouvement impossible")
      }
    }
    winner
  }

  val board  = initBoard()
  val winner = gameLoop(board)


}