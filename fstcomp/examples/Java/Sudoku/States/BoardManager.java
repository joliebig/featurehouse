import java.util.Stack;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.ObjectOutput;
import java.io.ObjectInput;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.Writer;

public class BoardManager {
    protected Stack history;

    public void preSetFieldWrapper(Structure structure, int structNr,
                                   int element, Field f) {
        try {
            history.push(board.clone());
        } catch (CloneNotSupportedException e) {

        }
        original(structure, structNr, element, f);
    }

    BoardManager() {
        history = new Stack();
    }

    public void preLoadWrapper() {
        history.clear();
        original();
    }

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
}