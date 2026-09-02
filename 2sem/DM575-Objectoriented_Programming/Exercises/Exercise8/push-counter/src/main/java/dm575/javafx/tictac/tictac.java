package dm575.javafx.tictac;

import javafx.application.Application;
import javafx.event.ActionEvent;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.stage.Stage;

public class tictac extends Application {
	// private gameConnector gameConnector = new gameConnector(); // Create an instance of the game connector
	private Board board = new Board(); // Create an instance of the board
	
	/**
	 * Presents a tic tac toe
	 */
	private GridPane gameGrid;
	private Text outputField;
	private boolean playerX = true; // true = X, false = O

	public void start(Stage stage) {		

		gameGrid = new GridPane();
		gameGrid.setAlignment(Pos.CENTER);
		outputField = new Text("Player X's turn");
		Button restartButton = new Button("Restart"); // Create a restart button
		restartButton.setOnAction(this::processButtonPressRestart); // Set the action for the restart button
	
		for (int row = 0; row < 3; row++) {
			for (int col = 0; col < 3; col++) {
				Button cell = new Button();
				int cellNumber = row * 3 + col + 1; // Calculate cell number (1-9)
				cell.setId(String.valueOf(cellNumber)); // Set the button's ID to the cell number
				cell.setOnAction(this::processButtonPressGrid); // Set the action for the button press
				cell.setMaxSize(70, 70);
				cell.setMinSize(70, 70);

				gameGrid.add(cell, col, row);
			}
		}

		VBox Game = new VBox(gameGrid, outputField, restartButton); // Create a VBox to hold the game grid and output field
		Game.setAlignment(Pos.CENTER);
		Scene scene = new Scene(Game, 250, 300);
	
		stage.setScene(scene);
		stage.setTitle("Tic Tac Toe");
		stage.setResizable(false);
		stage.setOnCloseRequest(e -> System.exit(0)); // Close the application when the window is closed
		stage.show();
	}

	public void processButtonPressRestart(ActionEvent event) {
		for (int row = 0; row < 3; row++) {
			for (int col = 0; col < 3; col++) {
				Button clickedButton = (Button) gameGrid.getChildren().get(row * 3 + col); // Get the button from the grid
				clickedButton.setText(""); // Clear the button text
				clickedButton.setStyle(""); // Reset the button style
			}
		}
		board = new Board(); // Reset the board
		outputField.setText("Player X's turn"); // Reset the output field
		playerX = true; // Reset the player turn
	}

	public void processButtonPressGrid(ActionEvent event) {

		Button clickedButton = (Button) event.getSource(); // Get the button that was just clicked
		int cellNumber = Integer.parseInt(clickedButton.getId()); // Get the cell number
		board.play(cellNumber); // Play the move on the board

		if (clickedButton.getText().equals("X") || clickedButton.getText().equals("O")) {
			return; // If the button is already clicked, do nothing
		}

		if (playerX) {
			clickedButton.setText("X"); // Set the button text to X
			clickedButton.setStyle("-fx-font-size: 24px; -fx-font-weight: bold;"); // Change font size and weight
			outputField.setText("Player O's turn");
			playerX = false; // Switch to player O
		} else {
			clickedButton.setText("O"); // Set the button text to O
			clickedButton.setStyle("-fx-font-size: 24px; -fx-font-weight: bold;"); // Change font size and weight
			outputField.setText("Player X's turn");
			playerX = true; // Switch to player X
		}
		if (board.won()) {
			if (playerX) {
				outputField.setText("Player O wins"); // Display the winner message
				for (int row = 0; row < 3; row++) {
					for (int col = 0; col < 3; col++) {
						Button loserButton = (Button) gameGrid.getChildren().get(row * 3 + col); // Get the button from the grid
						if (loserButton.getText().equals("X")) {
							loserButton.setStyle("-fx-font-size: 24px; -fx-text-fill: gray; -fx-font-weight: bold;"); // Change font size and text color to gray
						}
						
					}
				}
			} else {
				outputField.setText("Player X wins"); // Display the winner message
				for (int row = 0; row < 3; row++) {
					for (int col = 0; col < 3; col++) {
						Button loserButton = (Button) gameGrid.getChildren().get(row * 3 + col); // Get the button from the grid
						if (loserButton.getText().equals("O")) {
							loserButton.setStyle("-fx-font-size: 24px; -fx-text-fill: gray; -fx-font-weight: bold;"); // Change font size and text color to gray
						}
						
					}
				}
			}
			
		} 
		else if (board.tied()) {
			outputField.setText("Game tied!"); // Display a tie message
		}
	}

	public static void main(String[] args) {
		launch(args); // Launch the JavaFX application
	}
	
}