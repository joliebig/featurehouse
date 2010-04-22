

public class ListenerFactory {

    public ActionListener getGenerateSudokuListener() {
        return new GenerateSudokuListener(bm);
    }
}