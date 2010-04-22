
import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;


public class Field  implements Cloneable, Serializable {
    //private static final long serialVersionUID = 1L;

    public Object clone() throws CloneNotSupportedException {
        Field clone = new Field();
        clone.initialSet = initialSet;
        clone.set = set;
        clone.value = value;
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

}