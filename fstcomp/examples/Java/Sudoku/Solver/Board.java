import java.util.List;
import java.util.LinkedList;

public class Board {

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
}