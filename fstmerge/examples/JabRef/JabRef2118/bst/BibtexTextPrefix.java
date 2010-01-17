
package net.sf.jabref.bst; 



public  class  BibtexTextPrefix {
	

	
	public static String textPrefix(int numOfChars, String toPrefix, Warn warn) {

		StringBuffer sb = new StringBuffer();

		char[] cs = toPrefix.toCharArray();
		int n = cs.length;
		int i = 0;

		int braceLevel = 0;

		while (i < n && numOfChars > 0) {
			char c = cs[i];
			i++;
			if (c == '{') {
				braceLevel++;
				if (braceLevel == 1 && i < n && (cs[i] == '\\')) {
					i++; 
					while (i < n && braceLevel > 0) {
						if (cs[i] == '}') {
							braceLevel--;
						} else if (cs[i] == '{') {
							braceLevel++;
						}
						i++;
					}
					numOfChars--;
				}
			} else if (c == '}') {
				if (braceLevel > 0) {
					braceLevel--;
				} else {
					if (warn != null)
						warn.warn("Unbalanced brace in string for purify$: " + toPrefix);
				}
			} else {
				numOfChars--;
			}
			
		}
		sb.append(toPrefix.substring(0, i));
		while (braceLevel > 0){
			sb.append('}');
			braceLevel--;
		}

		return sb.toString();
	}



}
