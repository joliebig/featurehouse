

public class BoardManager {

    public void loadSudoku(Board board) {
        preLoadWrapper();
        this.board = board;
        updateSudokuViews();
    }
}