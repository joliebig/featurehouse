
package gnu.regexp;


public interface CharIndexed {
    
    char OUT_OF_BOUNDS = '\u';

    
    char charAt(int index);

    
    boolean move(int index);

    
    boolean isValid();
}
