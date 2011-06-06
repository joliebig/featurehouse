

package com.lowagie.text.rtf.list;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;

import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.RtfExtendedElement;
import com.lowagie.text.rtf.document.RtfDocument;



public class RtfListTable extends RtfElement implements RtfExtendedElement {

    
    protected static final byte[] LIST_NUMBER = "\\ls".getBytes();
    
    private static final byte[] LIST_TABLE = "\\*\\listtable".getBytes();
    
    private static final byte[] LIST = "\\list".getBytes();
    
    private static final byte[] LIST_TEMPLATE_ID = "\\listtemplateid".getBytes();
    
    private static final byte[] LIST_HYBRID = "\\hybrid".getBytes();
    
    private static final byte[] LIST_ID = "\\listid".getBytes();
    
    private static final byte[] LIST_OVERRIDE_TABLE = "\\*\\listoverridetable".getBytes();
    
    private static final byte[] LIST_OVERRIDE = "\\listoverride".getBytes();
    
    private static final byte[] LIST_OVERRIDE_COUNT = "\\listoverridecount".getBytes();
    
    
    private ArrayList<RtfList> lists;
    
    
    public RtfListTable(RtfDocument doc) {
        super(doc);
        
        this.lists = new ArrayList<RtfList>();
    }

    
    public void writeContent(final OutputStream out) throws IOException
    {        
    }
    
    
    public void writeDefinition(final OutputStream result) throws IOException
    {
        final int[] listIds = new int[lists.size()];
        result.write(OPEN_GROUP);
        result.write(LIST_TABLE);
        result.write("\n".getBytes());
        for(int i = 0; i < lists.size(); i++) {
            result.write(OPEN_GROUP);
            result.write(LIST);
            result.write(LIST_TEMPLATE_ID);
            result.write(intToByteArray(document.getRandomInt()));
            result.write(LIST_HYBRID);
            result.write("\n".getBytes());
            final RtfList rList = lists.get(i); 
            rList.writeDefinition(result);
            result.write(LIST_ID);
            listIds[i] = document.getRandomInt();
            result.write(intToByteArray(listIds[i]));
            result.write(CLOSE_GROUP);
            result.write("\n".getBytes());
        }
        result.write(CLOSE_GROUP);
        result.write("\n".getBytes());
        result.write(OPEN_GROUP);
        result.write(LIST_OVERRIDE_TABLE);
        result.write("\n".getBytes());
        for(int i = 0; i < lists.size(); i++) {
            result.write(OPEN_GROUP);
            result.write(LIST_OVERRIDE);
            result.write(LIST_ID);
            result.write(intToByteArray(listIds[i]));
            result.write(LIST_OVERRIDE_COUNT);
            result.write(intToByteArray(0));
            result.write(LIST_NUMBER);
            result.write(intToByteArray(lists.get(i).getListNumber()));
            result.write(CLOSE_GROUP);
            result.write("\n".getBytes());
        }
        result.write(CLOSE_GROUP);
        result.write("\n".getBytes());        
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
