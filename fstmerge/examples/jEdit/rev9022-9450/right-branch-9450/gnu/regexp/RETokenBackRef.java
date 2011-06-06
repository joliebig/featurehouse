

package gnu.regexp;

final class RETokenBackRef extends REToken {
  private int num;
  private boolean insens;
  
  RETokenBackRef(int subIndex, int num, boolean insens) {
    super(subIndex);
    this.num = num;
    this.insens = insens;
  }

  

    boolean match(CharIndexed input, REMatch mymatch) {
	int b,e;
	b = mymatch.start[num];
	e = mymatch.end[num];
	if ((b==-1)||(e==-1)) return false; 
	for (int i=b; i<e; i++) {
	    if (input.charAt(mymatch.index+i-b) != input.charAt(i)) {
		return false;
	    }
	}
	mymatch.index += e-b;
	return next(input, mymatch);
    }
    
    void dump(StringBuffer os) {
	os.append('\\').append(num);
    }
}


