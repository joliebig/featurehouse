
package gnu.regexp;
import java.io.Serializable;

class CharIndexedString implements CharIndexed, Serializable {
    private String s;
    private int anchor;
    private int len;
    
    CharIndexedString(String str, int index) {
	s = str;
	len = s.length();
	anchor = index;
    }

    public char charAt(int index) {
	int pos = anchor + index;
	return ((pos < len) && (pos >= 0)) ? s.charAt(pos) : OUT_OF_BOUNDS;
    }
    
    public boolean isValid() {
	return (anchor < len);
    }
    
    public boolean move(int index) {
	return ((anchor += index) < len);
    }
}
