

package gnu.regexp;

class RETokenStart extends REToken {
    private String newline; 
    
    RETokenStart(int subIndex, String newline) {
	super(subIndex);
	this.newline = newline;
    }
    
    boolean match(CharIndexed input, REMatch mymatch) {
	
	
	
	if (newline != null) {
	    int len = newline.length();
	    if (mymatch.offset >= len) {
		boolean found = true;
		char z;
		int i = 0; 
		char ch = input.charAt(mymatch.index - len);
		do {
		    z = newline.charAt(i);
		    if (ch != z) {
			found = false;
			break;
		    }
		    ++i;
		    ch = input.charAt(mymatch.index - len + i);
		} while (i < len);
	    
		if (found) return next(input, mymatch);
	    }
	}
	
	
	if ((mymatch.eflags & RE.REG_NOTBOL) > 0) return false;
	
	if ((mymatch.eflags & RE.REG_ANCHORINDEX) > 0)
	    return (mymatch.anchor == mymatch.offset) ? 
		next(input, mymatch) : false;
	else
	    return ((mymatch.index == 0) && (mymatch.offset == 0)) ?
		next(input, mymatch) : false;
    }
    
    void dump(StringBuffer os) {
	os.append('^');
    }
}
