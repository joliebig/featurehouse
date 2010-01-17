package net.sf.jabref; 

import java.util.HashSet; 
import java.util.Iterator; 


public  class  DuplicateCheck {
	


    
    public static boolean isDuplicate(BibtexEntry one, BibtexEntry two) {

        
        if (one.getType() != two.getType())
            return false;

        
        String[] fields = one.getType().getRequiredFields();

        float req, reqWeight = 2;
        if (fields == null) {
            req = 0;
            reqWeight = 0;
        }
        else
            req = compareFieldSet(fields, one, two);
        fields = one.getType().getOptionalFields();

        if (fields != null) {
            float opt = compareFieldSet(fields, one, two);
            return (reqWeight * req + opt) / (1 + reqWeight) >= Globals.duplicateThreshold;
        } else {
            return (req >= Globals.duplicateThreshold);
        }
    }


	

    private static float compareFieldSet(String[] fields, BibtexEntry one, BibtexEntry two) {
        int res = 0, empty = 0;
        for (int i = 0; i < fields.length; i++) {
            
            int result = compareSingleField(fields[i], one, two);
            if (result == Util.EQUAL) {
                res++;
                
            }
            else if (result == Util.EMPTY_IN_BOTH)
                empty++;
        }
        if (fields.length > empty)
            return ((float) res) / ((float) (fields.length - empty));
        else 
            return 0.5f;
    }


	

    private static int compareSingleField(String field, BibtexEntry one, BibtexEntry two) {
        String s1 = one.getField(field), s2 = two.getField(field);
        if (s1 == null) {
            if (s2 == null)
                return Util.EMPTY_IN_BOTH;
            else
                return Util.EMPTY_IN_ONE;
        } else if (s2 == null)
            return Util.EMPTY_IN_TWO;
        s1 = s1.toLowerCase();
        s2 = s2.toLowerCase();
        
        if (field.equals("author") || field.equals("editor")) {
            
            
            String[] aus1 = AuthorList.fixAuthor_lastNameFirst(s1).split(" and "), aus2 = AuthorList
                    .fixAuthor_lastNameFirst(s2).split(" and "), au1 = aus1[0].split(","), au2 = aus2[0]
                    .split(",");

            
            if ((aus1.length > 0) && (aus1.length == aus2.length)
                    && au1[0].trim().equals(au2[0].trim()))
                return Util.EQUAL;
            else
                return Util.NOT_EQUAL;
        } else {
            if (s1.trim().equals(s2.trim()))
                return Util.EQUAL;
            else
                return Util.NOT_EQUAL;
        }

    }


	

    public static double compareEntriesStrictly(BibtexEntry one, BibtexEntry two) {
        HashSet<String> allFields = new HashSet<String>();
        allFields.addAll(one.getAllFields());
        allFields.addAll(two.getAllFields());

        int score = 0;
        for (Iterator<String> fld = allFields.iterator(); fld.hasNext();) {
            String field = fld.next();
            Object en = one.getField(field), to = two.getField(field);
            if ((en != null) && (to != null) && (en.equals(to)))
                score++;
            else if ((en == null) && (to == null))
                score++;
        }
        if (score == allFields.size())
            return 1.01; 
            
            
        else
            return ((double) score) / allFields.size();
    }


	

    
    public static BibtexEntry containsDuplicate(BibtexDatabase database, BibtexEntry entry) {
        for (BibtexEntry other : database.getEntries()) {
            if (isDuplicate(entry, other))
                return other; 
        }
        return null; 
	}



}
