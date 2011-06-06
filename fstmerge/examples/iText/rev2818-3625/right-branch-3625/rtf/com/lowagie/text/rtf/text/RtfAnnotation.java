

package com.lowagie.text.rtf.text;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Annotation;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.document.RtfDocument;



public class RtfAnnotation extends RtfElement {

    
    private static final byte[] ANNOTATION_ID = "\\*\\atnid".getBytes();
    
    private static final byte[] ANNOTATION_AUTHOR = "\\*\\atnauthor".getBytes();
    
    private static final byte[] ANNOTATION = "\\*\\annotation".getBytes();
    
    
    private String title = "";
    
    private String content = "";
    
    
    public RtfAnnotation(RtfDocument doc, Annotation annotation) {
        super(doc);
        title = annotation.title();
        content = annotation.content();
    }
    
    
    public void writeContent(final OutputStream result) throws IOException
    {
        result.write(OPEN_GROUP);
        result.write(ANNOTATION_ID);
        result.write(DELIMITER);
        result.write(intToByteArray(document.getRandomInt()));
        result.write(CLOSE_GROUP);
        result.write(OPEN_GROUP);
        result.write(ANNOTATION_AUTHOR);
        result.write(DELIMITER);
        result.write(title.getBytes());
        result.write(CLOSE_GROUP);
        result.write(OPEN_GROUP);
        result.write(ANNOTATION);
        result.write(RtfParagraph.PARAGRAPH_DEFAULTS);
        result.write(DELIMITER);
        result.write(content.getBytes());
        result.write(CLOSE_GROUP);        
    }
}
