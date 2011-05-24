

import java.util.Random;

public class Board {

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