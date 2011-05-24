

public class BoardManager {
    public void undo() {
        if (!history.empty()) {
            board = (Board) history.pop();
            updateSudokuViews();
        }
    }
}