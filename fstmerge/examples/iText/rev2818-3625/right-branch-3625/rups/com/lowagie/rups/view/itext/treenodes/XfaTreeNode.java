

package com.lowagie.rups.view.itext.treenodes;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Enumeration;

import com.lowagie.rups.io.OutputStreamResource;
import com.lowagie.text.pdf.PRStream;
import com.lowagie.text.pdf.PdfReader;


public class XfaTreeNode extends FormTreeNode implements OutputStreamResource {

    
    public static final byte[] BOUNDARY_START = "<!--\nRUPS XFA individual packet: end of [".getBytes();
    
    public static final byte[] BOUNDARY_MIDDLE = "]; start of [".getBytes();
    
    public static final byte[] BOUNDARY_END = "]\n-->".getBytes();
    
    
    public XfaTreeNode(PdfObjectTreeNode xfa) {
        super(xfa);
    }
    
    
    @SuppressWarnings("unchecked")
    public void writeTo(OutputStream os) throws IOException {
        Enumeration<FormTreeNode> children = this.children();
        FormTreeNode node;
        PRStream stream;
        String key = null;
        String tmp = null;
        while (children.hasMoreElements()) {
            node = children.nextElement();
            if (key != null) {
                os.write(BOUNDARY_START);
                os.write(key.getBytes());
                os.write(BOUNDARY_MIDDLE);
                tmp = (String)node.getUserObject();
                os.write(tmp.getBytes());
                os.write(BOUNDARY_END);
            }
            key = tmp;
            stream = (PRStream)node.getCorrespondingPdfObjectNode().getPdfObject();
            os.write(PdfReader.getStreamBytes(stream));
        }
        os.flush();
        os.close();
    }

    
    public void addPacket(String key, PdfObjectTreeNode value) {
        FormTreeNode node = new FormTreeNode(value);
        node.setUserObject(key);
        this.add(node);
    }
    
    
    private static final long serialVersionUID = 2463297568233643790L;

}