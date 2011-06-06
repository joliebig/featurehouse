

package gnu.regexp;

final class RETokenAny extends REToken {
  
  private boolean newline; 

  
  private boolean matchNull;    
  
  RETokenAny(int subIndex, boolean newline, boolean matchNull) { 
    super(subIndex);
    this.newline = newline;
    this.matchNull = matchNull;
  }

  int getMinimumLength() {
    return 1;
  }

    boolean match(CharIndexed input, REMatch mymatch) {
    char ch = input.charAt(mymatch.index);
    if ((ch == CharIndexed.OUT_OF_BOUNDS)
	|| (!newline && (ch == '\n'))
	|| (matchNull && (ch == 0))) {
	return false;
    }
    ++mymatch.index;
    return next(input, mymatch);
  }

  void dump(StringBuffer os) {
    os.append('.');
  }
}

