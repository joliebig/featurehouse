

import java.util.List;
import java.util.LinkedList;

public class Guesser {

    public List guess(Board board) {
        // find unset field with the least possibilities, i.e. struct and field index by iterating over rows
        int structIndex = -1;
        int elementIndex = -1;
        for (int i = 0; i < Field.POSSIBILITIES; i++)
            for (int j = 0; j < Field.POSSIBILITIES; j++)
                if (!board.getField(Structure.ROW, i, j).isSet()
                        && ((-1 == structIndex && -1 == elementIndex)
                            || (board.getField(Structure.ROW, i, j).getRemainingPos().size()
                                < board.getField(Structure.ROW, structIndex, elementIndex)
                                .getRemainingPos().size()))) {
                    structIndex=i;
                    elementIndex=j;
                }
        List guessed = new LinkedList();
        // guess all possibilities
        for (int i = 0; i < board.getField(Structure.ROW, structIndex, elementIndex)
                .getRemainingPos().size(); i++) {
            try {
                Board guess = (Board) board.clone();
                if (guess.trySetField(Structure.ROW, structIndex, elementIndex,
                                      new Field((Integer) board.getField(Structure.ROW, structIndex, elementIndex)
                                                .getRemainingPos().get(i))))
                    guessed.add(guess);
            } catch (CloneNotSupportedException e) {
            }

        }
        return guessed;
    }

}