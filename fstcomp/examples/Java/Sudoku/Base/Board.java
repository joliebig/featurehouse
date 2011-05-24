/**
 * Implementation of a State
 *
 */
public class Board {

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
}
