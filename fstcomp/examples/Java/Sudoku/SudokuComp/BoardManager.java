import java.util.LinkedList; 
import java.util.List; 
import java.io.BufferedReader; 
import java.io.FileReader; 
import java.io.IOException; 
import java.io.File; import java.util.Stack; 
import java.io.FileNotFoundException; 
import java.io.FileOutputStream; 
import java.io.FileInputStream; 
import java.io.FileWriter; 
import java.io.ObjectOutput; 
import java.io.ObjectInput; 
import java.io.ObjectOutputStream; 
import java.io.ObjectInputStream; 
import java.io.Writer; import javax.swing.JOptionPane; 

public   class  BoardManager {
	
    protected Board board;

	

    protected List sudokuViews;

	

    public BoardManager  () {
        sudokuViews = new LinkedList();
    
        history = new Stack();
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

	

    private void   preSetFieldWrapper__wrappee__Base  (Structure structure, int structNr, int element, Field f) {

    }

	

    public void preSetFieldWrapper(Structure structure, int structNr,
                                   int element, Field f) {
        try {
            history.push(board.clone());
        } catch (CloneNotSupportedException e) {

        }
        preSetFieldWrapper__wrappee__Base(structure, structNr, element, f);
    }

	

    private void   preLoadWrapper__wrappee__Base  () {

    }

	

    public void preLoadWrapper() {
        history.clear();
        preLoadWrapper__wrappee__Base();
    }

	

    public void setField(Structure structure, int structNr, int element, Field f) {
        preSetFieldWrapper(structure, structNr, element, f);
        setFieldPrivate(structure, structNr, element, f);
        updateSudokuViews();
    }

	

    public Field getField(Structure structure, int structNr, int element) {
        return getBoard().getField(structure, structNr, element);
    }

	

    public Board getBoard  () {
        return this.board;
    }

	

    public void setBoard(Board board) {
        this.board = board;
    }

	
    protected Stack history;

	

    public void loadState(File f) throws IOException, ClassNotFoundException {
        ObjectInput i = new ObjectInputStream(new FileInputStream(f));
        board = (Board) i.readObject();
        history = (Stack) i.readObject();
        updateSudokuViews();
    }

	

    public void saveState(File f) throws IOException {
        ObjectOutput o = new ObjectOutputStream(new FileOutputStream(f));
        o.writeObject(getBoard());
        o.writeObject(history);
        o.close();
    }

	
    public void undo() {
        if (!history.empty()) {
            board = (Board) history.pop();
            updateSudokuViews();
        }
    }

	

    protected void setBusy(boolean busy) {
        for (int i = 0; i < sudokuViews.size(); i++) {
            ((Gui) sudokuViews.get(i)).setBusy(busy);
        }
    }

	

    protected boolean busy;

	

    protected boolean trySetFieldPrivate(Structure structure, int structNr, int element, Field f) {
        return board.trySetField(structure, structNr, element, f);
    }

	


    public boolean trySetField(Structure structure, int structNr, int element, Field f) {
        preSetFieldWrapper(structure, structNr, element, f);
        boolean set = trySetFieldPrivate(structure, structNr, element, f);
        if (set) {
            updateSudokuViews();
            return true;
        } else {
            undo();
            return false;
        }
    }

	

    public boolean tryLoadFile(File f) throws IOException {
        preLoadWrapper();
        board = new Board();
        BufferedReader fileReader = new BufferedReader(new FileReader(f));
        
        int digit = (Field.POSSIBILITIES / 10) + 1;

        int row = 0;
        while (row < Field.POSSIBILITIES) {
            String sudokuLine = fileReader.readLine();
            int value;
            char c;

            //for (int i = 0; i < Field.POSSIBILITIES; i++) {
            //    c = sudokuLine.charAt(i);

            //    if (c != '.') {
            //        value = Integer.parseInt(Character.toString(c));
            //        if (!trySetFieldPrivate(Structure.ROW, row, i, new Field(value,true))) {
            //            board = null;
            //            updateSudokuViews();
            //            return false;
            //        }
            //    }            
            //}
            
            int extendedInt;
            char extendedC;
            
            if (digit == 1) {
	            for (int i = 0; i < Field.POSSIBILITIES; i++) {            	
	                c = sudokuLine.charAt(i);
	
	                if (c != '.') {
	                    value = Integer.parseInt(Character.toString(c));
	                    if (!trySetFieldPrivate(Structure.ROW, row, i, new Field(value,true))) {
                        	board = null;
                        	updateSudokuViews();
                        	return false;
                        }
	                }
	            }
            } else if (digit == 2) {            
				for (int i = 0; i < Field.POSSIBILITIES * digit; i = i + digit) {
					c = sudokuLine.charAt(i);
					
					extendedC = sudokuLine.charAt(i + 1);
					
					if (c != '.' && extendedC != '.') {
						value = Integer.parseInt(Character.toString(c)) * 10;
						extendedInt = Integer.parseInt(Character.toString(extendedC));
						value += extendedInt;
						if (!trySetFieldPrivate(Structure.ROW, row, (i/digit), new Field(value,true))) {
                        	board = null;
                        	updateSudokuViews();
                        	return false;
                        }						
					} 
				}
			}
            
            row++;
        }
        updateSudokuViews();
        return true;
    }

	



    public boolean solutionHint() {
        //new ForcedField().trySolve(board);
        //if (true) {
        //    updateSudokuViews();
        //    return true;
        //}
        if (board.isSolved())
            return true;
        try {
            setBusy(true);
            List solutions = solve((Board) board.clone());
            if (solutions.isEmpty()) {
                setBusy(false);
                return false;                
            }
            for (int i = 0; i < Field.POSSIBILITIES; i++)
                for (int j = 0; j < Field.POSSIBILITIES; j++)
                    if (!board.getField(Structure.ROW, i, j).isSet()
                            && ((Board) solutions.get(0)).getField(Structure.ROW, i, j).isSet()) {
                        trySetField(Structure.ROW, i, j,
                                    ((Board) solutions.get(0)).getField(Structure.ROW, i, j));
                        updateSudokuViews();
                        return true;
                    }
            setBusy(false);
        } catch (CloneNotSupportedException e) {

        }
        return false;

    }

	

    protected List solve(Board board) {
        List solutions = new LinkedList();
        List solvers = new LinkedList();
        solvers.add(new ForcedField());
        solvers.add(new ForcedNumber());
        Guesser guesser = new Guesser();
        for (int i = 0; i < solvers.size(); i++)
            if (!((Solver) solvers.get(i)).trySolve(board))
                return solutions;
        if (!board.isSolved()) {
            List guessed = guesser.guess(board);
            for (int i = 0; i < guessed.size(); i++)
                solutions.addAll(solve(((Board)guessed.get(i))));
        } else {
            solutions.add(board);
        }
        return solutions;
    }

	

    public void loadSudoku(Board board) {
        preLoadWrapper();
        this.board = board;
        updateSudokuViews();
    }

	

    public void setPossibilities(int possibilities) {
        Field.POSSIBILITIES = possibilities;
        this.board = null;
        updateSudokuViews();
    }


}
