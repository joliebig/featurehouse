

package com.lowagie.text.rtf.list;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;

import com.lowagie.text.DocWriter;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.RtfExtendedElement;
import com.lowagie.text.rtf.document.RtfDocument;



public class RtfListTable extends RtfElement implements RtfExtendedElement {
    
    private static final byte[] LIST_TABLE = DocWriter.getISOBytes("\\*\\listtable");

    
    private static final byte[] LIST_OVERRIDE_TABLE = DocWriter.getISOBytes("\\*\\listoverridetable");
    
    private static final byte[] LIST_OVERRIDE = DocWriter.getISOBytes("\\listoverride");
    
    private static final byte[] LIST_OVERRIDE_COUNT = DocWriter.getISOBytes("\\listoverridecount");
    

    
    private ArrayList lists;
    
    private ArrayList picturelists;
    
    
    public RtfListTable(RtfDocument doc) {
        super(doc);
        
        this.lists = new ArrayList();
        this.picturelists = new ArrayList();
    }

    
    public void writeContent(final OutputStream out) throws IOException
    {        
    }
    
    
    public void writeDefinition(final OutputStream result) throws IOException
    {
        result.write(OPEN_GROUP);
        result.write(LIST_TABLE);
        this.document.outputDebugLinebreak(result);
        
        for(int i = 0; i < picturelists.size(); i++) {
            RtfPictureList l = (RtfPictureList)picturelists.get(i);

            l.writeDefinition(result);
            this.document.outputDebugLinebreak(result);
        }

        for(int i = 0; i < lists.size(); i++) {
            RtfList l = (RtfList)lists.get(i);
            l.setID(document.getRandomInt());
            l.writeDefinition(result);
            this.document.outputDebugLinebreak(result);
        }
        result.write(CLOSE_GROUP);
        this.document.outputDebugLinebreak(result);
        
        result.write(OPEN_GROUP);
        result.write(LIST_OVERRIDE_TABLE);
        this.document.outputDebugLinebreak(result);
        
        
        
        
        for(int i = 0; i < lists.size(); i++) {
            result.write(OPEN_GROUP);
            result.write(LIST_OVERRIDE);
            result.write(RtfList.LIST_ID);
            result.write(intToByteArray( ((RtfList) lists.get(i)).getID() ));
            result.write(LIST_OVERRIDE_COUNT);
            result.write(intToByteArray(0));    
            result.write(RtfList.LIST_NUMBER);
            result.write(intToByteArray( ((RtfList) lists.get(i)).getListNumber()) );
            result.write(CLOSE_GROUP);
            this.document.outputDebugLinebreak(result);
        }
        result.write(CLOSE_GROUP);
        this.document.outputDebugLinebreak(result);
    }

    
    public int getListNumber(RtfList list) {
        if(lists.contains(list)) {
            return lists.indexOf(list);
        } else {
            lists.add(list);
            return lists.size();
        }
    }
    
    
    public void freeListNumber(RtfList list) {
        int i = lists.indexOf(list);
        if(i >= 0) {
            lists.remove(i);
        }
    }
}
