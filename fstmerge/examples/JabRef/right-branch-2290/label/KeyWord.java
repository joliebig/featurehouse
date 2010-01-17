
package net.sf.jabref.label;

import java.util.Hashtable ; 

public class KeyWord{

   
    private KeyWord(){
        
        setKeyWords() ; 
    }

    private static void setKeyWords(){
        keyWordTable.put("society","society") ; 
        keyWordTable.put("transaction","transaction") ; 
        keyWordTable.put("transactions","transactions") ; 
        keyWordTable.put( "journal" , "journal" )  ; 
        keyWordTable.put( "review" , "review" )  ; 
        keyWordTable.put( "revue" , "revue" )  ; 
        keyWordTable.put( "communication" , "communication" )  ; 
        keyWordTable.put( "communications" , "communications" )  ; 
        keyWordTable.put( "letters" , "letters" )  ; 
        keyWordTable.put( "advances" , "advances" )  ; 
        keyWordTable.put( "proceedings" , "proceedings" )  ; 
        keyWordTable.put( "proceeding" , "proceeding" )  ; 
        keyWordTable.put( "international" , "international" )  ; 
        keyWordTable.put( "joint" , "joint" )  ; 
        keyWordTable.put( "conference" , "conference" )  ; 
    }


    
    
    public static void addKeyWord(String newKeyWord){
		keyWordTable.put(newKeyWord,newKeyWord) ; 
    }

    public static String removeKeyWord(String newKeyWord){
		return (String) keyWordTable.remove(newKeyWord) ; 

    }

    
   public static boolean isKeyWord(String matchWord){
       if(keyWordTable.size()==0){
		   setKeyWords() ; 
       }
       if(keyWordTable.containsKey(matchWord.toLowerCase())) {
            return true ; 
       }
       return false ; 
   }

   public static boolean isKeyWordMatchCase(String matchWord){
       if(keyWordTable.size()==0){
		   setKeyWords() ; 
       }
       if(keyWordTable.containsKey(matchWord)) {
            return true ; 
       }
       return false ; 
   }

   private static Hashtable keyWordTable = new Hashtable() ; 

}

