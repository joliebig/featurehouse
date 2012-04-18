import java.io.FileNotFoundException;

public class CommentRemover {

    private StringBuffer fileContent;
    
    public void Run(StringBuffer buf) {
        fileContent = buf;
        
        //Remove Comments
        RemoveComments();
        
        //Leere Zeilen
        RemoveEmptyLines();
    }
 
    
    private boolean bInString = false;
    private boolean bInChar = false;
    private boolean bInSingleLineComment = false;
    private boolean bInMultiLineComment = false;

    char actChar = 0;
    char lastChar = 0;
    
    int actPos = 0;
    int deleteBegin = 0;    //Anfang des zu löschenden Bereichs
    
    private boolean InAnything()
    {
        return bInString || bInChar || bInSingleLineComment || bInMultiLineComment;
    }
    
    
    private void OnSlash()
    {
        //ende eines MultiLine-Kommentars (*/)
        if (bInMultiLineComment && lastChar=='*')
        {
            bInMultiLineComment = false;
            
            //den abschließenden / auch noch löschen
            actPos++;
            
            Delete();

        //aktuelles zeichen zurücksetzen, damit daraus kein //, oder ähnliches wird
        actChar = 0;
        }
        //begin eines SingleLine-Kommentars (//)
        else if (lastChar=='/' && !InAnything())
        {
            bInSingleLineComment = true;
            //ab erstem / alles löschen
            deleteBegin = actPos-1; 
        }
    }
    
    //Beginn eines MultiLine-Kommentars?
    private void OnStar()
    {
        if (lastChar=='/' && !InAnything())
        {
            bInMultiLineComment = true;
//          ab / alles löschen
            deleteBegin = actPos-1; 
        }
    }
    
    //Zeilenende -> Ende eines einfachen Kommentars?
    private void OnEOL()
    {
        if (bInSingleLineComment)
        {
            bInSingleLineComment = false;
            
            //Bei Windows-Zeilenumbruch \r\n nicht löschen
            if (lastChar == '\r')
                actPos--;
            Delete();
        }
    }
    
    //Anfang oder Ende eines Strings?
    private void OnDoubleQuote()
    {
        //Ende?
        if (bInString)
        {
            //nur wenn vorher kein '\' war
            if (lastChar!='\\')
                bInString = false;
        }
        else if (!InAnything())
        {
            bInString = true;
        }
    }
    
    //Anfang oder Ende eines char?
    private void OnQuote()
    {
        //Ende?
        if (bInChar)
        {
            //nur wenn vorher kein '\' war
            if (lastChar!='\\')
                bInChar = false;
        }
        else if (!InAnything())
        {
            bInChar = true;
        }
    }
    
    //Löscht den aktuellen Bereich
    private void Delete()
    {
        fileContent.delete(deleteBegin, actPos);
        
        //danach den cursor auf das letzte gültige zeichen setzen
        actPos = deleteBegin + 1;
        
        //lastChar wieder zurücksetzen
        lastChar = 0;
    } 
    
    private void RemoveComments()
    {
        actPos = -1;
        deleteBegin = 0;
        
        bInString = false;
        bInChar = false;
        bInSingleLineComment = false;
        bInMultiLineComment = false;

        actChar = 0;
        lastChar = 0;
        
        //über alle zeichen iterieren
        while (actPos < fileContent.length()-1)
        {
            actPos++;
            lastChar = actChar;
            actChar = fileContent.charAt(actPos);
            
            switch (actChar)
            {
            case '/' : OnSlash();break;
            case '\n': OnEOL();break;
            case '"' : OnDoubleQuote();break;
            case '\'': OnQuote();break;
            case '*': OnStar();break;
            }
        }
    }
        
    //leere Zeilen löschen (inkl. Tabs, etc)
    private void RemoveEmptyLines()
    {
        int start = 0;
        int end = 0;
        
        while (end!=-1)
        {
            start = fileContent.indexOf("\n",start);
            end = fileContent.indexOf("\n",start+1);
            if (start!=-1 && end!=-1)
            {
                String mid = fileContent.substring(start, end);
                mid = mid.trim();
                if (mid.length()==0)
                {
                    fileContent.delete(start, end);
                    start = 0;  //von vorn
                }
            }

            if (start >= 0)
                start++;
        }
        
        //Zum Schluss ist u.U. noch die erste Zeile nur '\n'
        if (fileContent.substring(0, 1).equals("\n"))
            fileContent.delete(0, 1);
        else if (fileContent.substring(0, 2).equals("\r\n"))
            fileContent.delete(0, 2);
    }
}
