

package gnu.regexp;
import java.util.Vector;

final class RETokenRepeated extends REToken {
    private REToken token;
    private int min,max;
    private boolean stingy;
    
    RETokenRepeated(int subIndex, REToken token, int min, int max) {
	super(subIndex);
	this.token = token;
	this.min = min;
	this.max = max;
    }

    
    void makeStingy() {
	stingy = true;
    }
    
    
    boolean isStingy() {
	return stingy;
    }
    
    
    int getMinimumLength() {
	return (min * token.getMinimumLength());
    }

    
    
    

    
    
    

    boolean match(CharIndexed input, REMatch mymatch) {
	
	int numRepeats = 0; 
	
	
	REMatch newMatch = mymatch;
	REMatch last = null;
	REMatch current;

	
	
	Vector positions = new Vector();
	positions.addElement(newMatch);
	
	
	REMatch doables;
	REMatch doablesLast;
	REMatch recurrent;

	do {
	    
	    if (stingy && (numRepeats >= min)) {
		REMatch result = matchRest(input, newMatch);
		if (result != null) {
		    mymatch.assignFrom(result);
		    return true;
		}
	    }

	    doables = null;
	    doablesLast = null;

	    
	    for (current = newMatch; current != null; current = current.next) {
		recurrent = (REMatch) current.clone();
		if (token.match(input, recurrent)) {
		    
		    if (doables == null) {
			doables = recurrent;
			doablesLast = recurrent;
		    } else {
			
			
			doablesLast.next = recurrent;
		    }
		    
		    while (doablesLast.next != null) {
			doablesLast = doablesLast.next;
		    }
		}
	    }
	    
	    if (doables == null) break;
	    
	    
	    newMatch = doables;
	    
	    
	    ++numRepeats;
	    
	    positions.addElement(newMatch);
	} while (numRepeats < max);
	
	
	if (numRepeats < min) return false;
	
	
	int posIndex = positions.size();
	
	
	
	REMatch allResults = null;
	REMatch allResultsLast = null;

	REMatch results = null;
	while (--posIndex >= min) {
	    newMatch = (REMatch) positions.elementAt(posIndex);
	    results = matchRest(input, newMatch);
	    if (results != null) {
		if (allResults == null) {
		    allResults = results;
		    allResultsLast = results;
		} else {
		    
		    
		    allResultsLast.next = results;
		}
		
		while (allResultsLast.next != null) {
		    allResultsLast = allResultsLast.next;
		}
	    }
	    
	}
	if (allResults != null) {
	    mymatch.assignFrom(allResults); 
	    return true;
	}
	
	return false;
    }

    private REMatch matchRest(CharIndexed input, final REMatch newMatch) {
	REMatch current, single;
	REMatch doneIndex = null;
	REMatch doneIndexLast = null;
	
	for (current = newMatch; current != null; current = current.next) {
	    
	    single = (REMatch) current.clone();
	    if (next(input, single)) {
		
		if (doneIndex == null) {
		    doneIndex = single;
		    doneIndexLast = single;
		} else {
		    doneIndexLast.next = single;
		}
		
		while (doneIndexLast.next != null) {
		    doneIndexLast = doneIndexLast.next;
		}
	    }
	}
	return doneIndex;
    }

    void dump(StringBuffer os) {
	os.append("(?:");
	token.dumpAll(os);
	os.append(')');
	if ((max == Integer.MAX_VALUE) && (min <= 1))
	    os.append( (min == 0) ? '*' : '+' );
	else if ((min == 0) && (max == 1))
	    os.append('?');
	else {
	    os.append('{').append(min);
	    if (max > min) {
		os.append(',');
		if (max != Integer.MAX_VALUE) os.append(max);
	    }
	    os.append('}');
	}
	if (stingy) os.append('?');
    }
}
