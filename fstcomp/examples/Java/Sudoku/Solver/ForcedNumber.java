public class ForcedNumber implements Solver {

    public boolean trySolve(Board board) {
        // iterate over
        // structure types
        for (int i = 0; i < Structure.values().length; i ++) {
            // structure indices
            for (int j = 0; j < Field.POSSIBILITIES; j++) {
                // possibilities
                for (int k = 1; k <= Field.POSSIBILITIES; k++) {
                    int counter = 0;
                    int element = -1;
                    // structure elements
                    for (int l = 0; l < Field.POSSIBILITIES; l++) {
                        if (board.getField(Structure.values()[i], j, l).getRemainingPos()
                                .contains((Object) k)) {
                            counter++;
                            element = l;
                        }
                        if (counter > 1)
                            break;
                    }
                    if (counter == 1
                            && !board.getField(Structure.values()[i], j, element).isSet()
                            && !board.trySetField(Structure.values()[i], j, element, new Field(k)))
                        return false;
                }
            }
        }
        return true;
    }
}