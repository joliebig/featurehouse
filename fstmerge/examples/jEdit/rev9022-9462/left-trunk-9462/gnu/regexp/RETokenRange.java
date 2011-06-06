

package gnu.regexp;

final class RETokenRange extends REToken {
  private char lo, hi;
  private boolean insens;

  RETokenRange(int subIndex, char lo, char hi, boolean ins) {
    super(subIndex);
    this.lo = (insens = ins) ? Character.toLowerCase(lo) : lo;
    this.hi = ins ? Character.toLowerCase(hi) : hi;
  }

  int getMinimumLength() {
    return 1;
  }

    boolean match(CharIndexed input, REMatch mymatch) {
	char c = input.charAt(mymatch.index);
	if (c == CharIndexed.OUT_OF_BOUNDS) return false;
	if (insens) c = Character.toLowerCase(c);
	if ((c >= lo) && (c <= hi)) {
	    ++mymatch.index;
	    return next(input, mymatch);
	}
	return false;
    }
    
  void dump(StringBuffer os) {
    os.append(lo).append('-').append(hi);
  }
}

