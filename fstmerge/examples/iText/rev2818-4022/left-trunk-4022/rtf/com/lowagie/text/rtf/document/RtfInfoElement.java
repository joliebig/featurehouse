

package com.lowagie.text.rtf.document;

import java.io.IOException;
import java.io.OutputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import com.lowagie.text.DocWriter;
import com.lowagie.text.Meta;
import com.lowagie.text.rtf.RtfElement;



public class RtfInfoElement extends RtfElement {

    
    private static final byte[] INFO_AUTHOR = DocWriter.getISOBytes("\\author");
    
    private static final byte[] INFO_SUBJECT = DocWriter.getISOBytes("\\subject");
    
    private static final byte[] INFO_KEYWORDS = DocWriter.getISOBytes("\\keywords");
    
    private static final byte[] INFO_TITLE = DocWriter.getISOBytes("\\title");
    
    private static final byte[] INFO_PRODUCER = DocWriter.getISOBytes("\\operator");
    
    private static final byte[] INFO_CREATION_DATE =DocWriter.getISOBytes( "\\creationdate");

    
    private int infoType = -1;
    
    private String content = "";
    
    
    public RtfInfoElement(RtfDocument doc, Meta meta) {
        super(doc);
        infoType = meta.type();
        content = meta.getContent();
    }
    
        
    public void writeContent(final OutputStream result) throws IOException
    {
        result.write(OPEN_GROUP);
        switch(infoType) {
            case Meta.AUTHOR:
                result.write(INFO_AUTHOR);
                break;
            case Meta.SUBJECT:
                result.write(INFO_SUBJECT);
                break;
            case Meta.KEYWORDS:
                result.write(INFO_KEYWORDS);
                break;
            case Meta.TITLE:
                result.write(INFO_TITLE);
                break;
            case Meta.PRODUCER:
                result.write(INFO_PRODUCER);
                break;
            case Meta.CREATIONDATE:
                result.write(INFO_CREATION_DATE);
                break;
            default:
                result.write(INFO_AUTHOR);
                break;
        }
        result.write(DELIMITER);
        if(infoType == Meta.CREATIONDATE) {
            result.write(DocWriter.getISOBytes(convertDate(content)));
        } else {
            document.filterSpecialChar(result, content, false, false);
        }
        result.write(CLOSE_GROUP);
    }        
    
    
    private String convertDate(String date) {
        SimpleDateFormat sdf = new SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy");
        try {
            Date creationDate = sdf.parse(date);
            sdf = new SimpleDateFormat("\\'yr'yyyy\\'mo'MM\\'dy'dd\\'hr'HH\\'min'mm\\'sec'ss");
            return sdf.format(creationDate);
        } catch(ParseException pe) {
            pe.printStackTrace();
            return "";
        }
    }
}
