import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;


public class Board implements Cloneable, Serializable {
    //private static final long serialVersionUID = 1L;

    public Object clone() throws CloneNotSupportedException {
        Board clone = new Board();
        for (int i = 0; i < board.length; i++) {
            clone.board[i] = (Field) board[i].clone();
        }
        return clone;
    }

    private void writeObject(ObjectOutputStream aOutputStream) throws IOException {
        aOutputStream.writeObject(board);
        aOutputStream.defaultWriteObject();
    }

    private void readObject(ObjectInputStream aInputStream) throws ClassNotFoundException, IOException {
        aInputStream.defaultReadObject();
        board = (Field[]) aInputStream.readObject();
    }

}