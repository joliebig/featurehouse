
package net.sf.jabref.label;

import java.util.StringTokenizer;

import net.sf.jabref.BibtexEntry;

public class BookLabelRule extends DefaultLabelRule {

    
    
    public String applyRule(BibtexEntry oldEntry){
        String newLabel = "" ;

        StringTokenizer authorTokens = null ;
        
        try{
            if((String) oldEntry.getField("author")!= null){
                authorTokens= new StringTokenizer((String) oldEntry.getField("author"),",") ;
            }else
            if((String) oldEntry.getField("editor")!= null){
                authorTokens= new StringTokenizer((String) oldEntry.getField("editor"),",") ;
            }
            newLabel += authorTokens.nextToken().toLowerCase() ;
        }catch(Throwable t){
                        System.out.println("error getting author/editor: "+t) ;
        }

        
        try{
            if( oldEntry.getField("year")!= null){
                newLabel += String.valueOf( oldEntry.getField("year")) ;
            }
        }catch(Throwable t){
                        System.out.println("error getting author: "+t) ;
        }

        newLabel += "book" ;

        
        return newLabel;
    }
















}



