

package gnu.regexp;
import java.io.Serializable;


public final class REMatch implements Serializable, Cloneable {
    private String matchedText;

    
    int eflags; 

    
    
    int offset;

    
    
    
    int anchor;

    
    int index; 
    int[] start; 
    int[] end;   
    REMatch next; 

    public Object clone() {
	try {
	    REMatch copy = (REMatch) super.clone();
	    copy.next = null;

	    copy.start = (int[]) start.clone();
	    copy.end = (int[]) end.clone();

	    return copy;
	} catch (CloneNotSupportedException e) {
	    throw new Error(); 
	}
    }

    void assignFrom(REMatch other) {
	start = other.start;
	end = other.end;
	index = other.index;
	
	next = other.next;
    }

    REMatch(int subs, int anchor, int eflags) {
	start = new int[subs+1];
	end = new int[subs+1];
	this.anchor = anchor;
	this.eflags = eflags;
	clear(anchor);
    }

    void finish(CharIndexed text) {
	start[0] = 0;
	StringBuffer sb = new StringBuffer();
	int i;
	for (i = 0; i < end[0]; i++)
	    sb.append(text.charAt(i));
	matchedText = sb.toString();
	for (i = 0; i < start.length; i++) {
	    
	    
	    if ((start[i] == -1) ^ (end[i] == -1)) {
		start[i] = -1;
		end[i] = -1;
	    }
	}
	next = null; 
    }
    
    
    void clear(int index) {
	offset = index;
	this.index = 0;
	for (int i = 0; i < start.length; i++) {
	    start[i] = end[i] = -1;
	}
	next = null; 
    }
    
    
    public String toString() {
	return matchedText;
    }
    
    
    public int getStartIndex() {
	return offset + start[0];
    }
    
    
    public int getEndIndex() {
	return offset + end[0];
    }
  
    
    public String toString(int sub) {
	if ((sub >= start.length) || (start[sub] == -1)) return "";
	return (matchedText.substring(start[sub],end[sub]));
    }
    
    
    public int getSubStartIndex(int sub) {
	if (sub >= start.length) return -1;
	int x = start[sub];
	return (x == -1) ? x : offset + x;
    }
    
    
    public int getStartIndex(int sub) {
	if (sub >= start.length) return -1;
	int x = start[sub];
	return (x == -1) ? x : offset + x;
    }
  
    
    public int getSubEndIndex(int sub) {
	if (sub >= start.length) return -1;
	int x = end[sub];
	return (x == -1) ? x : offset + x;
    }
    
    
    public int getEndIndex(int sub) {
	if (sub >= start.length) return -1;
	int x = end[sub];
	return (x == -1) ? x : offset + x;
    }
    
    
    public String substituteInto(String input) {
	
	StringBuffer output = new StringBuffer();
	int pos;
	for (pos = 0; pos < input.length()-1; pos++) {
	    if ((input.charAt(pos) == '$') && (Character.isDigit(input.charAt(pos+1)))) {
		int val = Character.digit(input.charAt(++pos),10);
		if (val < start.length) {
		    output.append(toString(val));
		} 
	    } else output.append(input.charAt(pos));
	}
	if (pos < input.length()) output.append(input.charAt(pos));
	return output.toString();
    }
}
