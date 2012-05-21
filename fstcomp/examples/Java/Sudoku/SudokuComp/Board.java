import java.io.Serializable; 
import java.io.ObjectOutputStream; 
import java.io.IOException; 
import java.io.ObjectInputStream; import java.util.List; 
import java.util.LinkedList; 

import java.util.Random; 

public   class  Board  implements Cloneable, Serializable {
	

    /**
     * Number of elements of the Sudoku
     */
    public static int ELEMENTS = Field.POSSIBILITIES * Field.POSSIBILITIES;

	

    protected Field[] board;

	

    /**
     *
     */
    public Board() {
        this.board = new Field[ELEMENTS];
        for (int i = 0; i < ELEMENTS; i++) {
            this.board[i] = new Field();
        }
    }

	

    /**
     * Resolves field addresses.
     *
     * @param struct
     * @param structNr
     * @param element
     * @return
     */
    public Field getField(Structure struct, int structNr, int element) {
        return board[getIndex(struct, structNr, element)];
    }

	

    protected int getIndex(Structure str, int nr, int ele) {
        int sqrt = (int) Math.round(Math.sqrt(Field.POSSIBILITIES));

        if (str.name().equals("COL"))
            return nr + (ele * Field.POSSIBILITIES);
        else if (str.name().equals("ROW"))
            return (nr * Field.POSSIBILITIES) + ele;
        else if (str.name().equals("BOX"))
            return Field.POSSIBILITIES * (nr / sqrt * sqrt + ele / sqrt) + (nr % sqrt * sqrt + ele % sqrt);
        else
            return -1;
    }

	

    public void setField(Structure structure, int structNr, int element, Field f) {
        board[getIndex(structure, structNr, element)] = f;
    }

	
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

	

    public boolean isSolved() {
        for (int i = 0; i < board.length; i++)
            if (!board[i].isSet())
                return false;
        return true;
    }

	

    public boolean trySetField(Structure str, int strIndex, int element, Field f) {
        boolean validRemoveAction = removeValueFromStructures(getIndex(str, strIndex, element), f.getValue());
        if (validRemoveAction && getField(str, strIndex, element).getRemainingPos().contains((Object) f.getValue())) {
            this.setField(str, strIndex, element, f);
            return true;
        } else
            return false;
    }

	

    protected boolean removeValueFromStructures(int index, int value) {
        List relatedFieldIndices = getRelatedFieldIndices(index);
        for (int i = 0; i < relatedFieldIndices.size(); i++) {
            if (!board[(Integer) relatedFieldIndices.get(i)].isSet()) {
                List remainingPos = board[(Integer) relatedFieldIndices.get(i)].getRemainingPos();
                if (remainingPos.contains(value) && remainingPos.size() <= 1)
                    return false;
                remainingPos.remove((Object)value);
                board[(Integer) relatedFieldIndices.get(i)] = new Field(remainingPos);
            }
        }
        return true;
    }

	

    protected int getStructureIndex(int index, Structure str) {
        int sqrt = (int) Math.round(Math.sqrt(Field.POSSIBILITIES));

        if (str.name().equals("ROW"))
            return index / Field.POSSIBILITIES;
        else if (str.name().equals("COL"))
            return index % Field.POSSIBILITIES;
        else if (str.name().equals("BOX"))
            return sqrt * (index / (sqrt * Field.POSSIBILITIES)) + (index % Field.POSSIBILITIES) / sqrt;
        else
            return -1;
    }

	

    protected List getRelatedFieldIndices(int index) {
        List indices = new LinkedList();
        Structure str;
        int strIndex;
        int indexProcessing;
        for (int i = 0; i < Structure.values().length; i++) {
            str = Structure.values()[i];
            strIndex = getStructureIndex(index, str);
            for (int j = 0; j < Field.POSSIBILITIES; j++) {
                indexProcessing = getIndex(str, strIndex, j);
                if (! (indices.contains(indexProcessing) || indexProcessing == index)) {
                    indices.add(indexProcessing);
                }
            }
        }
        return indices;
    }

	

    public void removeRandomSetField(){
    
        Random r = new Random();
        int size = Field.POSSIBILITIES*Field.POSSIBILITIES;
        int rIndex = r.nextInt(size);
        int counter = 0;
        while( (board[rIndex].value <= 0) && counter < size){
            rIndex = ((rIndex + counter) % size);
            counter++;
        }
        
        //rIndex known
        //recreate field

        Board output = new Board();
    
        for(int i=0;i<Field.POSSIBILITIES;i++){
            for(int j=0;j<Field.POSSIBILITIES;j++){
                if(getIndex(Structure.ROW, i, j) != rIndex){
                        Field f = getField(Structure.ROW, i, j);
                        if(f.isSet())
                            output.trySetField(Structure.ROW, i, j, new Field(f.getValue()));
                }
            }
        }
    
        board = output.board;
        
    }


}
