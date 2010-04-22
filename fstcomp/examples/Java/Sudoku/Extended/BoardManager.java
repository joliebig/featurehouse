

public class BoardManager {

    public void setPossibilities(int possibilities) {
        Field.POSSIBILITIES = possibilities;
        this.board = null;
        updateSudokuViews();
    }

}