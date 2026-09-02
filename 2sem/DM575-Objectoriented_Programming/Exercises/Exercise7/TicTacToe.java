package Exercise7;

import java.util.Scanner;

public class TicTacToe {
    public static void main(String[] args) {
        Board board = new Board();
        Scanner scanner = new Scanner(System.in);

        // While the game is still ongoing...
        while (!board.tied() && !board.won()) {
            // Print the state of the board
            printState(board);

            // Try to read an integer
            if (scanner.hasNextInt()) {
                int move = scanner.nextInt();
                // Check that integer is a legal move
                if (move > 0 && move < 10 && board.isFree(move)) {
                    // Play it!
                    board.play(move);
                }
            } else {
                /* If the user entered something that's not an integer,
                   read whatever they entered and discard it. */ 
                scanner.next();
            }
        }
        printResult(board);
        scanner.close();
    }

    // Helper methods for printing the state of the board and the result.
    private static void printState (Board board) {
        board.print();
        System.out.println("Next player: " + board.nextPlayer());
    }

    private static void printResult (Board board) {
        board.print();
        if (board.tied()) {
            System.out.println("Game tied!");
        } else if (board.nextPlayer() == Player.O) {
            System.out.println("X won!");
        } else {
            /* If the game is not tied and X didn't win, 
               it's fair to assume that O won. */
            System.out.println("O won!");
        }
    }
}
