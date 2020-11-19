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

  def win(board:Array[Array[Int]], x:Int, y:Int, player:Int): Boolean ={
    var stopX = false
    var stopY = false
    var stopXY = false
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


      //On va checker biais droite et gauche
      if ((nbAlignes < 4)&& stopY){
        nbAlignes = 0
        newX = x
        newY = y


        // On va checker les biais droite et gauche
        while ((nbAlignes < 4) && !stopXY ) {
          if (newY < 6 && newY >= 0 && newX < 6 && newX >= 0) {
            if (board(newX)(newY) == player) {
              nbAlignes += 1
              newY += sensY
              newX += sensX
              println("x=" + newY + " y=" + newX + " nb=" + nbAlignes+" (b)")
            }
            else if (sensY == 1 && sensX == 1) {
              newY = y - 1
              newX = x - 1
              sensY = -1
              sensX = -1
            }

            else if (sensY == -1 && sensX == -1) {
              nbAlignes = 0
              newY = y
              newX = x
              sensX = 1
              sensY = -1
            }
            else if (sensY == -1 && sensX == 1) {
              newY = y + 1
              newX = x - 1
              sensX = -1
              sensY = 1
            }
            else stopXY = true

          }
          else stopXY = true


        }
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

  def determineAIdrop(board:Array[Array[Int]],player:Int) : Int ={
    var col = 0

    //On checke si la colonne est pleine
    while ((determineDrop(board, col+1) == null) && col<6 )col += 1

    // On checke si qqn gagne au prochain tour et si c'est le cas on bloque / gagne
    for (i <- board.indices){
      val height = determineDrop(board, i+1)
      if  ((height != null) && (win(board, height.asInstanceOf[Int], i , 1) || win(board, height.asInstanceOf[Int], i , 2)) )col  = i
    }
    col+1
  }

  def gameLoop(board:Array[Array[Int]]): Int ={
    var winner = 0
    var i,j = 0
    println("Combien de joueurs ?")
    val nbPlayer = scala.io.StdIn.readInt()
    if ((nbPlayer > 2) || (nbPlayer < 0)) throw new Error("nombre de joueurs imvalide")
    val player1isAI = nbPlayer == 0
    val player2isAI = nbPlayer <= 1
    var playerPlaying = 1
    var col =0




    while (winner ==0) {

      println("JOUEUR : "+playerPlaying)
      for (i <- board.indices) {
        for (j <- board(0).indices) {
          print(board(i)(j) + " ")
        }
        println()
      }

      // Proposer un move au joueur
      var height: Any = null
      while (height == null) {

        if ((playerPlaying == 1 && !player1isAI) || (playerPlaying == 2 && !player2isAI) ) {

          println("Quelle action voulez vous faire ?")
          col = scala.io.StdIn.readInt()
          println()
        }

        else {
          // Le joueur est une IA
          col = determineAIdrop(board, playerPlaying)
        }

        height = determineDrop(board, col)

        if (height != null) {
            board(height.asInstanceOf[Int])(col - 1) = playerPlaying
            val won = win(board, height.asInstanceOf[Int], col - 1, playerPlaying)
            println(won)
            if (won) {
              winner = playerPlaying
              println("La partie est terminée")
              println("Le joueur " + winner + " a gagné")

              if (player1isAI && player2isAI) {
                println("A STRANGE GAME")
                println("THE ONLY WINNING MOVE IS")
                println("NOT TO PLAY.\n")
                println("HOW ABOUT A NICE GAME OF CHESS?")
              }

            }
          } else println("Mouvement impossible")
        }
        if (winner == 0) playerPlaying = (playerPlaying%2) +1

      }

    winner
  }

  val board  = initBoard()
  val winner = gameLoop(board)


}