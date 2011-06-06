

package gnu.regexp;

final class RETokenEndSub extends REToken {
    RETokenEndSub(int subIndex) {
	super(subIndex);
    }
    
    boolean match(CharIndexed input, REMatch mymatch) {
	mymatch.end[subIndex] = mymatch.index;
	return next(input, mymatch);
    }
    
    void dump(StringBuffer os) {
	
    }
}
