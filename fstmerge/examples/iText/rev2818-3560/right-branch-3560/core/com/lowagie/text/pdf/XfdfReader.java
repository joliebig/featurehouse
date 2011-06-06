

package com.lowagie.text.pdf;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Stack;

import com.lowagie.text.xml.simpleparser.SimpleXMLDocHandler;
import com.lowagie.text.xml.simpleparser.SimpleXMLParser;


public class XfdfReader implements SimpleXMLDocHandler {
    
    private boolean foundRoot = false;
    private Stack<String> fieldNames = new Stack<String>();
    private Stack<String> fieldValues = new Stack<String>();

    
    HashMap<String, String>    fields;
    
    
    String    fileSpec;
    
       
    public XfdfReader(String filename) throws IOException {
        FileInputStream fin = null;
        try {
            fin = new FileInputStream(filename);
            SimpleXMLParser.parse(this, fin);
        }
        finally {
            try{if (fin != null) {fin.close();}}catch(Exception e){}
        }
    }
    
        
    public XfdfReader(byte xfdfIn[]) throws IOException {
        SimpleXMLParser.parse( this, new ByteArrayInputStream(xfdfIn));
   }
    
        
    public HashMap<String, String> getFields() {
        return fields;
    }
    
        
    public String getField(String name) {
        return fields.get(name);
    }
    
        
    public String getFieldValue(String name) {
        String field = fields.get(name);
        if (field == null)
            return null;
        else
            return field;
    }
    
        
    public String getFileSpec() {
        return fileSpec;
    }

        
    public void startElement(String tag, HashMap<String, String> h)
    {
        if ( !foundRoot ) {
            if (!tag.equals("xfdf"))
                throw new RuntimeException("Root element is not Bookmark.");
            else 
                foundRoot = true;
        }

        if ( tag.equals("xfdf") ){
            
        } else if ( tag.equals("f") ) {
            fileSpec = h.get( "href" );
        } else if ( tag.equals("fields") ) {
            fields = new HashMap<String, String>();        
        } else if ( tag.equals("field") ) {
            String    fName = h.get( "name" );
            fieldNames.push( fName );
        } else if ( tag.equals("value") ) {
            fieldValues.push( "" );
        }
    }
        
    public void endElement(String tag) {
        if ( tag.equals("value") ) {
            String    fName = "";
            for (int k = 0; k < fieldNames.size(); ++k) {
                fName += "." + fieldNames.elementAt(k);
            }
            if (fName.startsWith("."))
                fName = fName.substring(1);
            String    fVal = fieldValues.pop();
            fields.put( fName, fVal );
        }
        else if (tag.equals("field") ) {
            if (!fieldNames.isEmpty())
                fieldNames.pop();
        }
    }
    
        
    public void startDocument()
    {
        fileSpec = "";
    }
        
    public void endDocument()
    {
        
    }
        
    public void text(String str)
    {
        if (fieldNames.isEmpty() || fieldValues.isEmpty())
            return;
        
        String val = fieldValues.pop();
        val += str;
        fieldValues.push(val);
    }
}