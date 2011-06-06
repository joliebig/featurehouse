
package gnu.regexp;


final class RETokenLookAhead extends REToken
{
  REToken re;
  boolean negative;

  RETokenLookAhead(REToken re, boolean negative) throws REException {
    super(0);
    this.re = re;
    this.negative = negative;
  }

  boolean match(CharIndexed input, REMatch mymatch)
  {
    REMatch trymatch = (REMatch)mymatch.clone();
    REMatch trymatch1 = (REMatch)mymatch.clone();
    REMatch newMatch = null;
    if (re.match(input, trymatch)) {
      if (negative) return false;
      if (next(input, trymatch1))
        newMatch = trymatch1;
    }

    if (newMatch != null) {
      if (negative) return false;
      
      mymatch.assignFrom(newMatch);
      return true;
    }
    else { 
      if (negative)
        return next(input, mymatch);
      
      return false;
    }
  }

    void dump(StringBuffer os) {
	os.append("(?");
	os.append(negative ? '!' : '=');
	re.dumpAll(os);
	os.append(')');
    }
}

