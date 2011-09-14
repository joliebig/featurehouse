
import java.io.Serializable; 
import java.io.ObjectOutputStream; 
import java.io.IOException; 
import java.io.ObjectInputStream; import java.util.LinkedList; 
import java.util.List; 
import java.lang.Integer; 

public   class  Field   implements Cloneable, Serializable {
	

    /**
     * Numbers of sudoku
     */
    public static int POSSIBILITIES = 9;

	

    protected boolean set;

	

    protected boolean initialSet;

	

    protected int value;

	

    public Field  () {
        this.initialSet = false;
        this.set = false;
    
        remainingPos = new LinkedList();
        for (int i = 1; i <= POSSIBILITIES; i++) {
            remainingPos.add((Object) i);
        }
    }

	

    public Field  (int value, boolean initialSet) {
        this.value = value;
        this.set = true;
        this.initialSet = initialSet;
    
        remainingPos = new LinkedList();
    }

	

    public Field  (int value) {
        this.value = value;
        this.set = true;
        this.initialSet = false;
    
        remainingPos = new LinkedList();
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

	
    //private static final long serialVersionUID = 1L;

    private Object  clone__wrappee__Color  () throws CloneNotSupportedException {
        Field clone = new Field();
        clone.initialSet = initialSet;
        clone.set = set;
        clone.value = value;
        return clone;
    }

	

    public Object clone() throws CloneNotSupportedException {
        Field clone = (Field) clone__wrappee__Color();
        LinkedList remainingPosClone = new LinkedList();
        for (int i = 0; i < remainingPos.size(); i++) {
            remainingPosClone.add(new Integer(((Integer)remainingPos.get(i)).intValue()));
        }
        clone.remainingPos = remainingPosClone;
        return clone;
    }

	

    private void writeObject(ObjectOutputStream aOutputStream) throws IOException {
        aOutputStream.writeBoolean(set);
        aOutputStream.writeBoolean(initialSet);
        aOutputStream.writeInt(value);
        aOutputStream.defaultWriteObject();
    }

	

    private void readObject(ObjectInputStream aInputStream) throws ClassNotFoundException, IOException {
        aInputStream.defaultReadObject();
        set = aInputStream.readBoolean();
        initialSet = aInputStream.readBoolean();
        value = aInputStream.readInt();
    }

	

    protected List remainingPos;

	

    public Field(List remainingPos) {
        this.remainingPos = remainingPos;
    }

	
    /**
     *
     * @return
     */
    public List getRemainingPos() {
        return remainingPos;
    }

	

    // fürs Testen
    public String toString() {
        String output = "";
        if (remainingPos.isEmpty()) {
            output = "[" + value + "]";
        } else {
            output = "{";
            for (int i = 0; i < remainingPos.size(); i++) {
                output += remainingPos.get(i).toString();
            }
            output += "}";
        }
        return output;
    }

	

    public void setInitial(boolean flag){
        initialSet = flag;
    }


}
