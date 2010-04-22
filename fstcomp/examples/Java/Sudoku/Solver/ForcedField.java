public class ForcedField implements Solver {

    public boolean trySolve(Board board) {
        for (int i = 0; i < Field.POSSIBILITIES; i++)
            for (int j = 0; j < Field.POSSIBILITIES; j++)
                if ((board.getField(Structure.ROW, i, j).getRemainingPos().size() == 1)
                        && !board.getField(Structure.ROW, i, j).isSet())
                    if (!board.trySetField(Structure.ROW, i, j, new Field((Integer) board.getField(Structure.ROW, i, j).getRemainingPos().get(0))))
                        return false;
        return true;
    }
}