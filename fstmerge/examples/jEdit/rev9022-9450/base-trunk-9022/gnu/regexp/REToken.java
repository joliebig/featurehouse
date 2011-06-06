

package gnu.regexp;
import java.io.Serializable;

abstract class REToken implements Serializable {

  protected REToken next = null;
  protected REToken uncle = null;
  protected int subIndex;

  protected REToken(int subIndex) {
      this.subIndex = subIndex;
  }

  int getMinimumLength() {
    return 0;
  }

  void setUncle(REToken anUncle) {
    uncle = anUncle;
  }

    
    abstract boolean match(CharIndexed input, REMatch mymatch);
  
    
    protected boolean next(CharIndexed input, REMatch mymatch) {
	if (next == null) {
	    if (uncle == null) {
		return true;
	    } else {
		return uncle.match(input, mymatch);
	    }
	} else {
	    return next.match(input, mymatch);
	}
    }
  
  boolean chain(REToken token) {
      next = token;
      return true; 
  }

    abstract void dump(StringBuffer os);

  void dumpAll(StringBuffer os) {
    dump(os);
    if (next != null) next.dumpAll(os);
  }
}
