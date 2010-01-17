

package net.sf.jabref;

import java.util.Comparator;

public class BibtexStringComparator implements Comparator<BibtexString> {

    protected boolean considerRefs;
    private static final String MARKER = "__MARKER__";
    private static final String PADDED_MARKER = " "+MARKER+" ";

    
    public BibtexStringComparator(boolean considerRefs) {
        this.considerRefs = considerRefs;
    }

    public int compare(BibtexString s1, BibtexString s2) {

        
        if (considerRefs) {
            
            int ref1 = s1.getContent().replaceAll("#[A-Za-z]+#", PADDED_MARKER).split(MARKER).length,
                ref2 = s2.getContent().replaceAll("#[A-Za-z]+#", PADDED_MARKER).split(MARKER).length;

            if (ref1 != ref2)
                return ref1-ref2;
        }

        int res = 0;

        
        String name1 = s1.getName().toLowerCase(),
                name2 = s2.getName().toLowerCase();

        res = name1.compareTo(name2);

        if (res == 0)
            return res;

        
        
        if (considerRefs) {

            
            BibtexString pre, post;
            if (res < 0) {
                pre = s1;
                post = s2;
            } else {
                pre = s2;
                post = s1;
            }

            
            
            String namePost = post.getName().toLowerCase(),
                    textPre = pre.getContent().toLowerCase();

            
            if (textPre.indexOf("#" + namePost + "#") >= 0)
                res = -res;


        }

        return res;
    }

}
