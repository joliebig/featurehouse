
package com.lowagie.text.rtf.parser;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.EventListener;
import java.util.Iterator;
import java.util.Stack;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.rtf.direct.RtfDirectContent;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordData;
import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordListener;
import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordMgr;
import com.lowagie.text.rtf.parser.destinations.RtfDestination;
import com.lowagie.text.rtf.parser.destinations.RtfDestinationMgr;



public class RtfParser {
    
    private static final boolean debugParser = false;    
    private String logFile = null;
    private boolean logging = false;
    private boolean logAppend = false;
    
    
    private Element elem = null;
    
    private Document document = null;
    
    private RtfDocument rtfDoc = null;
    
    private RtfCtrlWordMgr rtfKeywordMgr = null;
    
    private RtfImportMgr importMgr = null;
    
    private RtfDestinationMgr destinationMgr = null;
    
    private Stack<RtfParserState> stackState = null;
        
    private RtfParserState currentState = null;
    
    private PushbackInputStream pbReader = null;
    
    private int conversionType = TYPE_IMPORT_FULL;
    

    
    
    

    
    public static final int PARSER_IN_HEADER = (0x0 << 28) | 0x000000;
    
    public static final int PARSER_IN_CHARSET = PARSER_IN_HEADER | 0x000001;
    
    public static final int PARSER_IN_DEFFONT = PARSER_IN_HEADER | 0x000002;
    
    public static final int PARSER_IN_FONT_TABLE = PARSER_IN_HEADER | 0x000003;
    
    public static final int PARSER_IN_FONT_TABLE_INFO = PARSER_IN_HEADER | 0x000004;
    
    public static final int PARSER_IN_FILE_TABLE = PARSER_IN_HEADER | 0x000005;
    
    public static final int PARSER_IN_COLOR_TABLE = PARSER_IN_HEADER | 0x000006;
    
    public static final int PARSER_IN_STYLESHEET = PARSER_IN_HEADER | 0x000007;
    
    public static final int PARSER_IN_LIST_TABLE = PARSER_IN_HEADER | 0x000008;
    
    public static final int PARSER_IN_LISTOVERRIDE_TABLE = PARSER_IN_HEADER | 0x000009;
    
    public static final int PARSER_IN_REV_TABLE = PARSER_IN_HEADER | 0x00000A;
    
    public static final int PARSER_IN_RSID_TABLE = PARSER_IN_HEADER | 0x0000B;
    
    public static final int PARSER_IN_GENERATOR = PARSER_IN_HEADER | 0x00000C;
    
    public static final int PARSER_IN_PARAGRAPH_TABLE = PARSER_IN_HEADER | 0x00000E;
    
    public static final int PARSER_IN_OLDCPROPS = PARSER_IN_HEADER | 0x00000F;
    
    public static final int PARSER_IN_OLDPPROPS = PARSER_IN_HEADER | 0x000010;
    
    public static final int PARSER_IN_OLDTPROPS = PARSER_IN_HEADER | 0x000012;
    
    public static final int PARSER_IN_OLDSPROPS = PARSER_IN_HEADER | 0x000013;
    
    public static final int PARSER_IN_PROT_USER_TABLE = PARSER_IN_HEADER | 0x000014;
    
    public static final int PARSER_IN_LATENTSTYLES = PARSER_IN_HEADER | 0x000015;
    
    public static final int PARSER_IN_PARAGRAPH_GROUP_PROPERTIES =PARSER_IN_HEADER | 0x000016;
    
    
    
    
    public static final int PARSER_IN_DOCUMENT = (0x2 << 28 ) | 0x000000;

    
    public static final int PARSER_IN_INFO_GROUP = PARSER_IN_DOCUMENT | 0x000001;

    
    public static final int PARSER_IN_UPR = PARSER_IN_DOCUMENT | 0x000002;
    
    public static final int PARSER_IN_SHPPICT = PARSER_IN_DOCUMENT | 0x000010; 
    
    public static final int PARSER_IN_PICT = PARSER_IN_DOCUMENT | 0x000011; 
    
    public static final int PARSER_IN_PICPROP = PARSER_IN_DOCUMENT | 0x000012; 
    
    public static final int PARSER_IN_BLIPUID = PARSER_IN_DOCUMENT | 0x000013; 

    
    
    public static final int PARSER_STARTSTOP = (0x4 << 28)| 0x0001;
    
    
    public static final int PARSER_ERROR = (0x8 << 28) | 0x0000;
    
    public static final int PARSER_ERROR_EOF = PARSER_ERROR | 0x0001;
    
    public static final int PARSER_IN_UNKNOWN = PARSER_ERROR | 0x0FFFFFFF;
    
    
    
    public static final int TYPE_UNIDENTIFIED = -1;
    
    public static final int TYPE_IMPORT_FULL = 0;
    
    public static final int TYPE_IMPORT_FRAGMENT = 1;
    
    public static final int TYPE_CONVERT = 2;
    
    public static final int TYPE_IMPORT_INTO_ELEMENT = 3;

    
    
    public static final int DESTINATION_NORMAL = 0;
    
    public static final int DESTINATION_SKIP = 1;
    
    
    
    
    
    public static final int TOKENISER_NORMAL = 0x00000000;
    
    public static final int TOKENISER_SKIP_BYTES = 0x00000001;
    
    public static final int TOKENISER_SKIP_GROUP = 0x00000002;
    
    public static final int TOKENISER_BINARY= 0x00000003;
    
    public static final int TOKENISER_HEX= 0x00000004;
    
    public static final int TOKENISER_IGNORE_RESULT= 0x00000005;
    
    public static final int TOKENISER_STATE_IN_ERROR =  0x80000000; 
    
    public static final int TOKENISER_STATE_IN_UNKOWN = 0xFF000000; 
    
    
    private int groupLevel = 0;
    
    private int docGroupLevel = 0;
    
    private long binByteCount = 0;
    
    private long binSkipByteCount = 0;
    
    private int skipGroupLevel = 0;

    
    public static final int  errOK =0;                        
    public static final int  errStackUnderflow   =  -1;       
    public static final int  errStackOverflow    =  -2;       
    public static final int  errUnmatchedBrace   =  -3;       
    public static final int  errInvalidHex       =  -4;       
    public static final int  errBadTable         =  -5;       
    public static final int  errAssertion        =  -6;       
    public static final int  errEndOfFile        =  -7;       
    public static final int  errCtrlWordNotFound =  -8;          
    
    
    
    
    
    private long byteCount = 0;
    
    private long ctrlWordCount = 0;
    
    private long openGroupCount = 0;
    
    private long closeGroupCount = 0;
    
    private long characterCount = 0;
    
    private long ctrlWordHandledCount = 0;
    
    private long ctrlWordNotHandledCount = 0;
    
    private long ctrlWordSkippedCount = 0;
    
    private long groupSkippedCount = 0;
    
    private long startTime = 0;
    
    private long endTime = 0;
    
    private Date startDate = null;
    
    private Date endDate = null;
    
    
    private RtfCtrlWordData lastCtrlWordParam = null;
    
    
    private ArrayList<EventListener> listeners = new ArrayList<EventListener>();
    
    
    public RtfParser(Document doc) {
        this.document = doc;
    }
    
    
    public void importRtfDocument(InputStream readerIn, RtfDocument rtfDoc) throws IOException {
        if(readerIn == null || rtfDoc == null) return;
        this.init(TYPE_IMPORT_FULL, rtfDoc, readerIn, this.document, null);
        this.setCurrentDestination(RtfDestinationMgr.DESTINATION_NULL);
        startDate = new Date();
        startTime = System.currentTimeMillis();
        this.groupLevel = 0;
        try {
            this.tokenise();
        } catch (RuntimeException e) {
            
            e.printStackTrace();
        }
        catch (Exception e) {
            
            e.printStackTrace();
        }
        endTime = System.currentTimeMillis();
        endDate = new Date();
    }
    
    public void importRtfDocumentIntoElement(Element elem, InputStream readerIn, RtfDocument rtfDoc) throws IOException {
        if(readerIn == null || rtfDoc == null || elem == null) return;
        this.init(TYPE_IMPORT_INTO_ELEMENT, rtfDoc, readerIn, this.document, elem);
        this.setCurrentDestination(RtfDestinationMgr.DESTINATION_NULL);
        startDate = new Date();
        startTime = System.currentTimeMillis();
        this.groupLevel = 0;
        try {
            this.tokenise();
        } catch (RuntimeException e) {
            
            e.printStackTrace();
        }
        catch (Exception e) {
            
            e.printStackTrace();
        }
        endTime = System.currentTimeMillis();
        endDate = new Date();
    }
    
    public void convertRtfDocument(InputStream readerIn, Document doc) throws IOException {
        if(readerIn == null || doc == null) return;
        this.init(TYPE_CONVERT, null, readerIn, doc, null);
        this.setCurrentDestination(RtfDestinationMgr.DESTINATION_DOCUMENT);
        startDate = new Date();
        startTime = System.currentTimeMillis();
        this.groupLevel = 0;
        this.tokenise();
        endTime = System.currentTimeMillis();
        endDate = new Date();
    }

    
    public void importRtfFragment(InputStream readerIn, RtfDocument rtfDoc, RtfImportMappings importMappings) throws IOException {
    
        if(readerIn == null || rtfDoc == null || importMappings==null) return;
        this.init(TYPE_IMPORT_FRAGMENT, rtfDoc, readerIn, null, null);
        this.handleImportMappings(importMappings);
        this.setCurrentDestination(RtfDestinationMgr.DESTINATION_DOCUMENT);
        this.groupLevel = 1;
        setParserState(RtfParser.PARSER_IN_DOCUMENT);
        startDate = new Date();
        startTime = System.currentTimeMillis();
        this.tokenise();
        endTime = System.currentTimeMillis();
        endDate = new Date();
    }
    
    

    
    public void addListener(EventListener listener) {
        listeners.add(listener);
    }

    
    public void removeListener(EventListener listener) {
        listeners.remove(listener);
    }

    
    private void init(int type, RtfDocument rtfDoc, InputStream readerIn, Document doc, Element elem) {

        init_stats();
        
        this.pbReader = init_Reader(readerIn);
        
        this.conversionType = type;
        this.rtfDoc = rtfDoc;
        this.document = doc;
        this.elem = elem;
        this.currentState = new RtfParserState();
        this.stackState = new Stack<RtfParserState>();
        this.setParserState(PARSER_STARTSTOP);
        this.importMgr = new RtfImportMgr(this.rtfDoc, this.document);

        
        this.destinationMgr = RtfDestinationMgr.getInstance(this);
        
        RtfDestinationMgr.setParser(this);


        
        









        
        this.rtfKeywordMgr = new RtfCtrlWordMgr(this, this.pbReader);
        
        Object listener;
        for (Iterator<EventListener> iterator = listeners.iterator(); iterator.hasNext();) {
            listener = iterator.next();
            if(listener instanceof RtfCtrlWordListener) {
                this.rtfKeywordMgr.addRtfCtrlWordListener((RtfCtrlWordListener)listener);    
            }
        }
























































































    }
    
    protected void init_stats() {
        byteCount = 0;
        ctrlWordCount = 0;
        openGroupCount = 0;
        closeGroupCount = 0;
        characterCount = 0;
        ctrlWordHandledCount = 0;
        ctrlWordNotHandledCount = 0;
        ctrlWordSkippedCount = 0;
        groupSkippedCount = 0;
        startTime = 0;
        endTime = 0;
        startDate = null;
        endDate = null;
    }
    
    
    private PushbackInputStream init_Reader(InputStream readerIn) {













        
        if(!(readerIn instanceof BufferedInputStream)) {
            readerIn = new BufferedInputStream(readerIn);
        }
        if(!(readerIn instanceof PushbackInputStream)) {
            readerIn = new PushbackInputStream(readerIn);
        }
        
        return  (PushbackInputStream)readerIn;
    }
    
    
    private void handleImportMappings(RtfImportMappings importMappings) {
        Iterator<String> it = importMappings.getFontMappings().keySet().iterator();
        while(it.hasNext()) {
            String fontNr = it.next();
            this.importMgr.importFont(fontNr, importMappings.getFontMappings().get(fontNr));
        }
        it = importMappings.getColorMappings().keySet().iterator();
        while(it.hasNext()) {
            String colorNr = it.next();
            this.importMgr.importColor(colorNr, importMappings.getColorMappings().get(colorNr));
        }
        it = importMappings.getListMappings().keySet().iterator();
        while(it.hasNext()) {
            String listNr = it.next();
            this.importMgr.importList(listNr, importMappings.getListMappings().get(listNr));
        }
        it = importMappings.getStylesheetListMappings().keySet().iterator();
        while(it.hasNext()) {
            String stylesheetListNr = it.next();
            this.importMgr.importStylesheetList(stylesheetListNr, importMappings.getStylesheetListMappings().get(stylesheetListNr));
        }
        
    }
    
    
    
    
    
    public int handleOpenGroup() {
        int result = errOK;
        this.openGroupCount++;    
        this.groupLevel++;        
        this.docGroupLevel++;    
        if (this.getTokeniserState() == TOKENISER_SKIP_GROUP) { 
            this.groupSkippedCount++;
        }
    
        RtfDestination dest = this.getCurrentDestination();
        boolean handled = false;
        
        if(dest != null) {
            if(debugParser) {
                RtfParser.outputDebug(this.rtfDoc, groupLevel, "DEBUG: before dest.handleOpeningSubGroup()");
                RtfParser.outputDebug(this.rtfDoc, groupLevel, "DEBUG: destination=" + dest.toString());
            }
            handled = dest.handleOpeningSubGroup();
            if(debugParser) {
                RtfParser.outputDebug(this.rtfDoc, groupLevel, "DEBUG: after dest.handleOpeningSubGroup()");
            }
        }

        this.stackState.push(this.currentState);
        this.currentState = new RtfParserState(this.currentState);
        
        
        this.currentState.newGroup = true;
        dest = this.getCurrentDestination();
        
        if(debugParser) {
            RtfParser.outputDebug(this.rtfDoc, groupLevel, "DEBUG: handleOpenGroup()");
            if(this.lastCtrlWordParam != null)
                RtfParser.outputDebug(this.rtfDoc, groupLevel, "DEBUG: LastCtrlWord=" + this.lastCtrlWordParam.ctrlWord);
            RtfParser.outputDebug(this.rtfDoc, groupLevel, "DEBUG: grouplevel=" + Integer.toString(groupLevel));
            RtfParser.outputDebug(this.rtfDoc, groupLevel, "DEBUG: destination=" + dest.toString());
        }

        if(dest != null) {
            handled = dest.handleOpenGroup();
        }
        
        if(debugParser) {
            RtfParser.outputDebug(this.rtfDoc, groupLevel, "DEBUG: after dest.handleOpenGroup(); handled=" + Boolean.toString(handled));
        }
        
        return result;
    }
    public static void outputDebug(Object doc, int groupLevel, String str) {
        System.out.println(str);
        if(doc == null) return;
        if(groupLevel<0) groupLevel = 0;
        char[] a; Arrays.fill(a= new char[groupLevel*2], ' ');
        String spaces= new String(a);
        if(doc instanceof RtfDocument) {
            ((RtfDocument)doc).add(new RtfDirectContent("\n" + spaces + str));
        }
        else
            if(doc instanceof Document) {
                try {
                    ((Document)doc).add(new RtfDirectContent("\n" + spaces + str));
                } catch (DocumentException e) {
                    
                    e.printStackTrace();
                }
            }
    }
    
    public int handleCloseGroup() {
        int result = errOK;
        this.closeGroupCount++;    

        if (this.getTokeniserState() != TOKENISER_SKIP_GROUP) {
            if(debugParser) {
                RtfParser.outputDebug(this.rtfDoc, groupLevel, "DEBUG: handleCloseGroup()");
                if(this.lastCtrlWordParam != null)
                    RtfParser.outputDebug(this.rtfDoc, groupLevel, "DEBUG: LastCtrlWord=" + this.lastCtrlWordParam.ctrlWord);
                RtfParser.outputDebug(this.rtfDoc, groupLevel, "DEBUG: grouplevel=" + Integer.toString(groupLevel));
                RtfParser.outputDebug(this.rtfDoc, groupLevel, "DEBUG: destination=" + this.getCurrentDestination().toString());
                RtfParser.outputDebug(this.rtfDoc, groupLevel, "");
            }
            RtfDestination dest = this.getCurrentDestination();
            boolean handled = false;
            
            if(dest != null) {
                handled = dest.handleCloseGroup();
            }
            if(debugParser) {
                RtfParser.outputDebug(this.rtfDoc, groupLevel, "DEBUG: After dest.handleCloseGroup(); handled = " + Boolean.toString(handled));
                RtfParser.outputDebug(this.rtfDoc, groupLevel, "");
            }
        }
        
        if(this.stackState.size() >0 ) {
            this.currentState = this.stackState.pop();
        } else {
            result = errStackUnderflow;
        }
        
        this.docGroupLevel--;
        this.groupLevel--;
        
        if (this.getTokeniserState() == TOKENISER_SKIP_GROUP && this.groupLevel < this.skipGroupLevel) {
            this.setTokeniserState(TOKENISER_NORMAL);
        }

        return result;    
    }
    

    
    public int handleCtrlWord(RtfCtrlWordData ctrlWordData) {
        int result = errOK;
        this.ctrlWordCount++; 

        if(debugParser) {
            RtfParser.outputDebug(this.rtfDoc, groupLevel, "DEBUG: handleCtrlWord=" + ctrlWordData.ctrlWord + " param=[" + ctrlWordData.param + "]");
        }

        if (this.getTokeniserState() == TOKENISER_SKIP_GROUP) { 
            this.ctrlWordSkippedCount++;
            if(debugParser) {
                RtfParser.outputDebug(this.rtfDoc, groupLevel, "DEBUG: SKIPPED");
            }
            return result;
        }

        




        
        result = this.rtfKeywordMgr.handleKeyword(ctrlWordData, this.groupLevel);

        if( result == errOK){
            this.ctrlWordHandledCount++;
        } else {
            this.ctrlWordNotHandledCount++;
            result = errOK;    
        }
        
        return result;
    }

    

    public int handleCharacter(int nextChar) {        
        this.characterCount++;    

        if (this.getTokeniserState() == TOKENISER_SKIP_GROUP) { 
            return errOK;
        }

        boolean handled = false;

        RtfDestination dest = this.getCurrentDestination();
        if(dest != null) {
            handled = dest.handleCharacter(nextChar);
        }

        return errOK;
    }

    
    public RtfParserState getState(){
        return this.currentState;
    }    

    
    public int getParserState(){
        return this.currentState.parserState;
    }
    
    
    public int setParserState(int newState){
        this.currentState.parserState = newState;
        return this.currentState.parserState;
    }

    
    public int getConversionType() {
        return this.conversionType;
    }
    
    
    public RtfDocument getRtfDocument() {
        return this.rtfDoc;
    }
    
    
    public Document getDocument() {
        return this.document;
    }

    
    public RtfImportMgr getImportManager() {
        return importMgr;
    }
    
    
    
    
    
    public boolean setCurrentDestination(String destination) {
            RtfDestination dest = RtfDestinationMgr.getDestination(destination);
            if(dest != null) {
                this.currentState.destination = dest;
                return false;
            } else {
                this.setTokeniserStateSkipGroup();
                return false;
            }
    }
    
    public RtfDestination getCurrentDestination() {
        return this.currentState.destination;
    }
    
    public RtfDestination getDestination(String destination) {
        return RtfDestinationMgr.getDestination(destination);
    }
    
    
    public boolean isNewGroup() {
        return this.currentState.newGroup;
    }
    
    public boolean setNewGroup(boolean value) {
        this.currentState.newGroup = value;
        return this.currentState.newGroup;
    }
    
    
    
        
    public void tokenise() throws IOException {
        int errorCode = errOK;    
        int nextChar = 0;


        this.setTokeniserState(TOKENISER_NORMAL);    
        
        

        while((nextChar = this.pbReader.read()) != -1) {
            this.byteCount++;
            
            if (this.getTokeniserState() == TOKENISER_BINARY)                      
            {
                if ((errorCode = parseChar(nextChar)) != errOK)
                    return; 
            }  else {

                switch(nextChar) {
                    case '{':    
                        this.handleOpenGroup();
                        break;
                    case '}':  
                        this.handleCloseGroup();
                        break;
                    case 0x0a:    
                    case 0x0d:    



                        break;
                    case '\\':    
                            if(parseCtrlWord(pbReader) != errOK) {
                            
                            return;
                        }
                        break;
                    default:
                        if(groupLevel == 0) { 
                            break;
                        }
                        if(this.getTokeniserState() == TOKENISER_HEX) {
                            StringBuffer hexChars = new StringBuffer();
                            hexChars.append(nextChar);

                            if((nextChar = pbReader.read()) == -1) {
                                return;
                            }
                            this.byteCount++;
                            hexChars.append(nextChar);
                            try {

                                nextChar=Integer.parseInt(hexChars.toString(), 16);
                            } catch (NumberFormatException e) {
                                return;
                            }
                            this.setTokeniserState(TOKENISER_NORMAL);
                        }
                        if ((errorCode = parseChar(nextChar)) != errOK) {
                            return; 
                                    
                        }
                        break;
                }    
            }    
            



            
        }
        RtfDestination dest = this.getCurrentDestination();
        if(dest != null) {
            dest.closeDestination();
        }
    }
    
    
    private int parseChar(int nextChar) {
        
        
        
        
        if (this.getTokeniserState() == TOKENISER_BINARY && --binByteCount <= 0)
            this.setTokeniserStateNormal();
        if (this.getTokeniserState() == TOKENISER_SKIP_BYTES && --binSkipByteCount <= 0)
            this.setTokeniserStateNormal();
        return this.handleCharacter(nextChar);
    }
    
    
    private int parseCtrlWord(PushbackInputStream reader) throws IOException {
        int nextChar = 0;
        int result = errOK;
        
        if((nextChar = reader.read()) == -1) {
            return errEndOfFile;
        }
        this.byteCount++;

        StringBuffer parsedCtrlWord = new StringBuffer();
        StringBuffer parsedParam= new StringBuffer();
        RtfCtrlWordData ctrlWordParam = new RtfCtrlWordData();
        
        if(!Character.isLetterOrDigit((char)nextChar)) {
            parsedCtrlWord.append((char)nextChar);
            ctrlWordParam.ctrlWord = parsedCtrlWord.toString();
            result =  this.handleCtrlWord(ctrlWordParam);
            lastCtrlWordParam = ctrlWordParam;
            return result;
        }
        
        do {
            parsedCtrlWord.append((char)nextChar);
            
            nextChar = reader.read();
            this.byteCount++;
        } while  (Character.isLetter((char)nextChar));
        
        ctrlWordParam.ctrlWord = parsedCtrlWord.toString();

        if(nextChar == '-') {
            ctrlWordParam.isNeg = true;
            if((nextChar = reader.read()) == -1) {
                    return errEndOfFile;
            }
            this.byteCount++;
        }
        

        if(Character.isDigit((char)nextChar)) {
            ctrlWordParam.hasParam = true;
            do {
                parsedParam.append((char)nextChar);
                
                nextChar = reader.read();
                this.byteCount++;
                } while  (Character.isDigit((char)nextChar));
                        
            ctrlWordParam.param = parsedParam.toString();
        }
        
        
        if(nextChar != ' ') { 
            reader.unread(nextChar);
        }
        
        if(debugParser) {
    
    
    
    



        }
        
        result = this.handleCtrlWord(ctrlWordParam);
        lastCtrlWordParam = ctrlWordParam;
        return result;

    }
    
    
    public int setTokeniserState(int value) {
        this.currentState.tokeniserState = value;
        return this.currentState.tokeniserState;
    }
    
    
    public int getTokeniserState() {
        return this.currentState.tokeniserState;
    }

    
    public int getLevel() {
        return this.groupLevel;
    }
    

    
    public void setTokeniserStateNormal() {
        this.setTokeniserState(TOKENISER_NORMAL);
    }

    
    public void setTokeniserStateSkipGroup() {
        this.setTokeniserState(TOKENISER_SKIP_GROUP);
        this.skipGroupLevel = this.groupLevel;
    }
    
    
    public void setTokeniserSkipBytes(long numberOfBytesToSkip) {
        this.setTokeniserState(TOKENISER_SKIP_BYTES);
        this.binSkipByteCount = numberOfBytesToSkip;
    }
    
    
    public void setTokeniserStateBinary(int binaryCount) {
        this.setTokeniserState(TOKENISER_BINARY);
        this.binByteCount = binaryCount;
    }
    
    public void setTokeniserStateBinary(long binaryCount) {
        this.setTokeniserState(TOKENISER_BINARY);
        this.binByteCount = binaryCount;
    }
    
    public boolean isConvert() {
        return (this.getConversionType() == RtfParser.TYPE_CONVERT);
    }
    
    
    public boolean isImport() {
        return (isImportFull() || this.isImportFragment());
    }
    
    public boolean isImportFull() {
        return (this.getConversionType() == RtfParser.TYPE_IMPORT_FULL);
    }
    
    public boolean isImportFragment() {
        return (this.getConversionType() == RtfParser.TYPE_IMPORT_FRAGMENT);
    }
    
    public boolean getExtendedDestination() {
        return this.currentState.isExtendedDestination;
    }
    
    public boolean setExtendedDestination(boolean value) {
        this.currentState.isExtendedDestination = value;
        return this.currentState.isExtendedDestination;
    }

    
    public String getLogFile() {
        return logFile;
    }

    
    public void setLogFile(String logFile) {
        this.logFile = logFile;
    }
    
    public void setLogFile(String logFile, boolean logAppend) {
        this.logFile = logFile;
        this.setLogAppend(logAppend);
    }

    
    public boolean isLogging() {
        return logging;
    }

    
    public void setLogging(boolean logging) {
        this.logging = logging;
    }

    
    public boolean isLogAppend() {
        return logAppend;
    }

    
    public void setLogAppend(boolean logAppend) {
        this.logAppend = logAppend;
    }


}
