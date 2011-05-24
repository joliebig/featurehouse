import java.util.LinkedList;
import java.util.List;
import java.lang.Integer;


public class Field {

    protected List remainingPos;

    public Field(List remainingPos) {
        this.remainingPos = remainingPos;
    }

    Field() {
        remainingPos = new LinkedList();
        for (int i = 1; i <= POSSIBILITIES; i++) {
            remainingPos.add((Object) i);
        }
    }

    Field(int value, boolean initialSet) {
        remainingPos = new LinkedList();
    }

    Field(int value) {
        remainingPos = new LinkedList();
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

    public Object clone() throws CloneNotSupportedException {
        Field clone = (Field) original();
        LinkedList remainingPosClone = new LinkedList();
        for (int i = 0; i < remainingPos.size(); i++) {
            remainingPosClone.add(new Integer(((Integer)remainingPos.get(i)).intValue()));
        }
        clone.remainingPos = remainingPosClone;
        return clone;
    }
}