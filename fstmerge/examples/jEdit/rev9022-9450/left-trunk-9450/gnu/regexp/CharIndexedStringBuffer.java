
package gnu.regexp;
import java.io.Serializable;

class CharIndexedStringBuffer implements CharIndexed, Serializable {
    private StringBuffer s;
    private int anchor;

    CharIndexedStringBuffer(StringBuffer str, int index) {
	s = str;
	anchor = index;
    }

  public char charAt(int index) {
      int pos = anchor + index;
    return ((pos < s.length()) && (pos >= 0)) ? s.charAt(pos) : OUT_OF_BOUNDS;
  }

  public boolean isValid() {
    return (anchor < s.length());
  }

  public boolean move(int index) {
    return ((anchor += index) < s.length());
  }
}
