
package gnu.regexp;
import java.io.Serializable;

class CharIndexedCharArray implements CharIndexed, Serializable {
    private char[] s;
    private int anchor;
    
    CharIndexedCharArray(char[] str, int index) {
	s = str;
	anchor = index;
    }
    
    public char charAt(int index) {
	int pos = anchor + index;
	return ((pos < s.length) && (pos >= 0)) ? s[pos] : OUT_OF_BOUNDS;
    }
    
    public boolean isValid() {
	return (anchor < s.length);
    }
    
    public boolean move(int index) {
	return ((anchor += index) < s.length);
    }
}
