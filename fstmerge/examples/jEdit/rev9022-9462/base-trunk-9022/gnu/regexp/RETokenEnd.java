
package gnu.regexp;

final class RETokenEnd extends REToken {
    
  private String newline;

  RETokenEnd(int subIndex,String newline) { 
    super(subIndex);
    this.newline = newline;
  }

    boolean match(CharIndexed input, REMatch mymatch) {
	char ch = input.charAt(mymatch.index);
	if (ch == CharIndexed.OUT_OF_BOUNDS)
	    return ((mymatch.eflags & RE.REG_NOTEOL)>0) ? 
		false : next(input, mymatch);
	if (newline != null) {
	    char z;
	    int i = 0; 
	    do {
		z = newline.charAt(i);
		if (ch != z) return false;
		++i;
		ch = input.charAt(mymatch.index + i);
	    } while (i < newline.length());
	    
	    return next(input, mymatch);
	}
	return false;
    }

  void dump(StringBuffer os) {
    os.append('$');
  }
}
