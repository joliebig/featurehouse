/**
 * Stores all possible numbers that can still be set on a sudoku field. A Field
 * can be set to final, only one possibility is left after that.
 *
 * Attention! There is no 0 in sudoku. All values are stored n-1, that means the
 * possibility 1 is stored as 0 in a Bitset. getFinal() can return 0 if no
 * possibility is left, but if that happens the sudoku cannot be solved.
 * getValue(0) is invalid!
 *
 */
public class Field {

    /**
     * Numbers of sudoku
     */
    public static int POSSIBILITIES = 9;

    protected boolean set;

    protected boolean initialSet;

    protected int value;

    /**
     *
     */
    public Field() {
        this.initialSet = false;
        this.set = false;
    }

    public Field(int value, boolean initialSet) {
        this.value = value;
        this.set = true;
        this.initialSet = initialSet;
    }

    public Field(int value) {
        this.value = value;
        this.set = true;
        this.initialSet = false;
    }

    /**
     *
     * @return
     */
    public int getValue() {
        return value;
    }

    /**
     *
     * @return
     */
    public boolean isInitialSet() {
        return initialSet;
    }

    /**
     *
     * @return
     */
    public boolean isSet() {
        return set;
    }

}