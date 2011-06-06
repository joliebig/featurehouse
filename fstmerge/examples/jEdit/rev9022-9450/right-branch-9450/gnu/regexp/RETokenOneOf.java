

package gnu.regexp;
import java.util.Vector;

final class RETokenOneOf extends REToken {
  private Vector options;
  private boolean negative;

  
  
  

  RETokenOneOf(int subIndex, String optionsStr, boolean negative, boolean insens) {
    super(subIndex);
    options = new Vector();
    this.negative = negative;
    for (int i = 0; i < optionsStr.length(); i++)
      options.addElement(new RETokenChar(subIndex,optionsStr.charAt(i),insens));
  }

  RETokenOneOf(int subIndex, Vector options, boolean negative) {
    super(subIndex);
    this.options = options;
    this.negative = negative;
  }

  int getMinimumLength() {
    int min = Integer.MAX_VALUE;
    int x;
    for (int i=0; i < options.size(); i++) {
      if ((x = ((REToken) options.elementAt(i)).getMinimumLength()) < min)
	min = x;
    }
    return min;
  }

    boolean match(CharIndexed input, REMatch mymatch) {
    if (negative && (input.charAt(mymatch.index) == CharIndexed.OUT_OF_BOUNDS)) 
      return false;

    REMatch newMatch = null;
    REMatch last = null;
    REToken tk;
    boolean isMatch;
    for (int i=0; i < options.size(); i++) {
	tk = (REToken) options.elementAt(i);
	REMatch tryMatch = (REMatch) mymatch.clone();
	if (tk.match(input, tryMatch)) { 
	    if (negative) return false;

	    if (next(input, tryMatch)) {
		
		if (last == null) {
		    newMatch = tryMatch;
		    last = tryMatch;
		} else {
		    last.next = tryMatch;
		    last = tryMatch;
		}
	    } 
	} 
    } 

    if (newMatch != null) {
	if (negative) {
	    return false;
	} else {
	    

	    
	    mymatch.assignFrom(newMatch);
	    return true;
	}
    } else {
	if (negative) {
	    ++mymatch.index;
	    return next(input, mymatch);
	} else {
	    return false;
	}
    }

    
  }

  void dump(StringBuffer os) {
    os.append(negative ? "[^" : "(?:");
    for (int i = 0; i < options.size(); i++) {
      if (!negative && (i > 0)) os.append('|');
      ((REToken) options.elementAt(i)).dumpAll(os);
    }
    os.append(negative ? ']' : ')');
  }  
}
