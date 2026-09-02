package dm575.javafx.tictac;
public class Board {
    private Player[] board;
    private Player nextPlayer;

    public Board() {
        board = new Player[9];
        nextPlayer = Player.X;
    }

    public Player[] getBoard() {
        return board;
    }

    public void print() {
        for(int i = 0; i < board.length; i++) {      
          if (board[i] != null) {
            System.out.print(board[i].name());
          } else {
            System.out.print(' ');
          }
          if (i % 3 < 2)
            System.out.print('|');
          else if (i < board.length - 1)
            System.out.println("\n-+-+-");
          else
            System.out.println(); 
        }
    }

    public boolean isFree (int square) {
        assert (square > 0 && square < 10);
        return (board[square-1] == null);
    }

    public Player nextPlayer() {
        return nextPlayer;
    }

    public boolean tied() {
        int free = 0;
        for(int i=0;i < board.length; i++)
          if (board[i] == null)
            free = free + 1; 
        return (free == 0) && !won();
    }

    public void play (int square) {
        System.out.println("Playing square " + square);
        assert (square > 0 && square < 10);
        board[square-1] = nextPlayer;
        if (nextPlayer == Player.X) {
            nextPlayer = Player.O;
        } else {
            nextPlayer = Player.X;
        }
        print();
    }

    public boolean won() {
        Player player = (nextPlayer() == Player.X) ? Player.O : Player.X;
        boolean notWon = true;
        // check row and col wins
        int i = 0;
        while (notWon && i < 3) {
          notWon = !(wonRow(player, i) || wonCol(player, i));
          i = i + 1;
        }
        // check diagonal win (unless notWon is false)
        notWon = notWon && !wonDiagonal(player);
        // check anti-diagonal win  (unless notWon is false)
        notWon = notWon && !wonAntiDiagonal(player);
        return !notWon;
    }

    // auxiliary methods for won()
    private boolean wonRow(Player player, int row) {
        int i = 0;
        boolean won = true;
        while ( won && i < 3 ) {
            won = player == board[ row * 3 + i];
            i = i + 1;
        }
        return won;
    }

    private boolean wonCol(Player player, int col) {
        int i = 0;
        boolean won = true;
        while ( won && i < 3 ) {
            won = player == board[ i * 3 + col ];
            i = i + 1;
        }
        return won;
    }

    private boolean wonDiagonal(Player player) {
        int i = 0;
        boolean won = true;
        while ( won && i < 3 ) {
            won = player == board[ i * 3 + i];
            i = i + 1;
        }
        return won;
    }

    private boolean wonAntiDiagonal(Player player) {
        int i = 0;
        boolean won = true;
        while ( won && i < 3 ) {
            won = player == board[ i * 3 + 2 - i];
            i = i + 1;
        }
        return won;
    }
}
