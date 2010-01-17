
package net.sf.jabref.label;

import net.sf.jabref.AuthorList;
import net.sf.jabref.BibtexEntry;
import net.sf.jabref.BibtexFields;

public class ArticleLabelRule extends DefaultLabelRule {

    
    
    public String applyRule(BibtexEntry oldEntry){
        String oldLabel = (oldEntry.getField(BibtexFields.KEY_FIELD)) ;
        String newLabel = "" ;

        String author="";

        
        try{
            author=oldEntry.getField("author");
            String[] tokens= author.split("\\band\\b");
            if( tokens.length > 0){ 
                if(tokens[0].indexOf(",") > 0)
                    tokens[0] = AuthorList.fixAuthor_firstNameFirst( tokens[0] ); 
                String[] firstAuthor = tokens[0].replaceAll("\\s+"," ").split(" ");
                

                newLabel += firstAuthor[ firstAuthor.length-1];
            }
        }catch(Throwable t){
            System.out.println("error getting author: "+t) ;
        }

        
        try{
            if( ! newLabel.equals("")){
                if( oldEntry.getField("year")!= null){
                    newLabel += String.valueOf( oldEntry.getField("year")) ;
                }
            }else
                newLabel=oldLabel; 
        }catch(Throwable t){
            System.out.println("error getting year: "+t) ;
        }





        return newLabel ;




    }















}



