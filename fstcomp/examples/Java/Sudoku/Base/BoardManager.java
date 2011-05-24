import java.util.LinkedList;
import java.util.List;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.File;

public class BoardManager {
    protected Board board;

    protected List sudokuViews;

    public BoardManager() {
        sudokuViews = new LinkedList();
    }

    public void registerSudokuView(Gui f) {
        sudokuViews.add(f);
        updateSudokuViews();
    }

    protected void updateSudokuViews() {
        for (int i = 0; i < sudokuViews.size(); i++) {
            ((Gui) sudokuViews.get(i)).update(getBoard());
        }
    }

    public void loadFile(File f) throws IOException {
        preLoadWrapper();
        board = new Board();
        BufferedReader fileReader = new BufferedReader(new FileReader(f));

		int digit = (Field.POSSIBILITIES / 10) + 1;		
	
        int row = 0;
        while (row < Field.POSSIBILITIES) {
            String sudokuLine = fileReader.readLine();
            int value;
            char c;
            
            int extendedInt;
            char extendedC;
            
            if (digit == 1) {
	            for (int i = 0; i < Field.POSSIBILITIES; i++) {            	
	                c = sudokuLine.charAt(i);
	
	                if (c != '.') {
	                    value = Integer.parseInt(Character.toString(c));
	                    setFieldPrivate(Structure.ROW, row, i, new Field(value,true));
	                }
	            }
            } else if (digit == 2) {            
				for (int i = 0; i < Field.POSSIBILITIES * digit; i = i + digit) {
					c = sudokuLine.charAt(i);
					extendedC = sudokuLine.charAt(i + 1);
					
					if (c != '.') {
						value = Integer.parseInt(Character.toString(c)) * 10;
						extendedInt = Integer.parseInt(Character.toString(extendedC));
						value += extendedInt;
						setFieldPrivate(Structure.ROW, row, (i/digit), new Field(value,true));						
					} 
				}
			}
			
            row++;
        }
        updateSudokuViews();
    }

    protected void setFieldPrivate(Structure structure, int structNr, int element, Field f) {
        board.setField(structure, structNr, element, f);
    }

    protected void preSetFieldWrapper(Structure structure, int structNr, int element, Field f) {

    }

    protected void preLoadWrapper() {

    }

    public void setField(Structure structure, int structNr, int element, Field f) {
        preSetFieldWrapper(structure, structNr, element, f);
        setFieldPrivate(structure, structNr, element, f);
        updateSudokuViews();
    }

    public Field getField(Structure structure, int structNr, int element) {
        return getBoard().getField(structure, structNr, element);
    }

    public Board getBoard() {
        if (board == null) {
            board = new Board();
        }
        return board;
    }

    public void setBoard(Board board) {
        this.board = board;
    }
}
