

package gnu.regexp;

final class RETokenChar extends REToken {
  private char[] ch;
  private boolean insens;

  RETokenChar(int subIndex, char c, boolean ins) {
    super(subIndex);
    ch = new char [1];
    ch[0] = (insens = ins) ? Character.toLowerCase(c) : c;
  }

  int getMinimumLength() {
    return ch.length;
  }
  
    boolean match(CharIndexed input, REMatch mymatch) {
	int z = ch.length;
	char c;
	for (int i=0; i<z; i++) {
	    c = input.charAt(mymatch.index+i);
	    if (( (insens) ? Character.toLowerCase(c) : c ) != ch[i]) {
		return false;
	    }
	}
	mymatch.index += z;

	return next(input, mymatch);
    }

  
  boolean chain(REToken next) {
    if (next instanceof RETokenChar) {
      RETokenChar cnext = (RETokenChar) next;
      
      int newsize = ch.length + cnext.ch.length;
      
      char[] chTemp = new char [newsize];
      
      System.arraycopy(ch,0,chTemp,0,ch.length);
      System.arraycopy(cnext.ch,0,chTemp,ch.length,cnext.ch.length);
      
      ch = chTemp;
      return false;
    } else return super.chain(next);
  }

  void dump(StringBuffer os) {
    os.append(ch);
  }
}


