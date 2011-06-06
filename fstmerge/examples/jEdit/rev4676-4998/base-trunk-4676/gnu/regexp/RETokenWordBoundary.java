

package gnu.regexp;


final class RETokenWordBoundary extends REToken {
    private boolean negated;
    private int where;
    static final int BEGIN = 1;
    static final int END = 2;

    RETokenWordBoundary(int subIndex, int where, boolean negated) {
	super(subIndex);
	this.where = where;
	this.negated = negated;
    }
    
    boolean match(CharIndexed input, REMatch mymatch) {
	
	
	
	
	
	
	boolean after = false;  
	boolean before = false; 
	char ch;

	
	if (((mymatch.eflags & RE.REG_ANCHORINDEX) != RE.REG_ANCHORINDEX) 
	    || (mymatch.offset + mymatch.index > mymatch.anchor)) {
	    if ((ch = input.charAt(mymatch.index - 1)) != CharIndexed.OUT_OF_BOUNDS) {
		before = Character.isLetterOrDigit(ch) || (ch == '_');
	    }
	}

	if ((ch = input.charAt(mymatch.index)) != CharIndexed.OUT_OF_BOUNDS) {
	    after = Character.isLetterOrDigit(ch) || (ch == '_');
	}

	
	
	boolean doNext = false;

	if ((where & BEGIN) == BEGIN) {
	    doNext = after && !before;
	}
	if ((where & END) == END) {
	    doNext ^= before && !after;
	}

	if (negated) doNext = !doNext;

	return (doNext ? next(input, mymatch) : false);
    }
    
    void dump(StringBuffer os) {
	if (where == (BEGIN | END)) {
	    os.append( negated ? "\\B" : "\\b" );
	} else if (where == BEGIN) {
	    os.append("\\<");
	} else {
	    os.append("\\>");
	}
    }
}
