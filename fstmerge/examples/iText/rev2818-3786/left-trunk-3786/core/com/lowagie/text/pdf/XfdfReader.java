

package com.lowagie.text.pdf;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Stack;

import com.lowagie.text.xml.simpleparser.SimpleXMLDocHandler;
import com.lowagie.text.xml.simpleparser.SimpleXMLParser;


public class XfdfReader implements SimpleXMLDocHandler {
    
    private boolean foundRoot = false;
    private Stack fieldNames = new Stack();
    private Stack fieldValues = new Stack();

    
    HashMap    fields;
    
    protected HashMap listFields;
    
    
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
    
        
    public HashMap getFields() {
        return fields;
    }
    
        
    public String getField(String name) {
        return (String)fields.get(name);
    }
    
        
    public String getFieldValue(String name) {
        String field = (String)fields.get(name);
        if (field == null)
            return null;
        else
            return field;
    }
    
        
    public List getListValues(String name) {
        return (List)listFields.get(name);
    }
    
        
    public String getFileSpec() {
        return fileSpec;
    }

        
    public void startElement(String tag, HashMap h)
    {
        if ( !foundRoot ) {
            if (!tag.equals("xfdf"))
                throw new RuntimeException("Root element is not Bookmark.");
            else 
                foundRoot = true;
        }

        if ( tag.equals("xfdf") ){
            
        } else if ( tag.equals("f") ) {
            fileSpec = (String)h.get( "href" );
        } else if ( tag.equals("fields") ) {
            fields = new HashMap();        
            listFields = new HashMap();
        } else if ( tag.equals("field") ) {
            String    fName = (String) h.get( "name" );
            fieldNames.push( fName );
        } else if ( tag.equals("value") ) {
            fieldValues.push( "" );
        }
    }
        
    public void endElement(String tag) {
        if ( tag.equals("value") ) {
            String    fName = "";
            for (int k = 0; k < fieldNames.size(); ++k) {
                fName += "." + (String)fieldNames.elementAt(k);
            }
            if (fName.startsWith("."))
                fName = fName.substring(1);
            String fVal = (String) fieldValues.pop();
            String old = (String) fields.put( fName, fVal );
            if (old != null) {
                List l = (List) listFields.get(fName);
                if (l == null) {
                    l = new ArrayList();
                    l.add(old);
                }
                l.add(fVal);
                listFields.put(fName, l);
            }
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
        
        String val = (String)fieldValues.pop();
        val += str;
        fieldValues.push(val);
    }
}