package com.lowagie.text.rtf.graphic;

import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Iterator;

import com.lowagie.text.DocWriter;
import com.lowagie.text.rtf.RtfAddableElement;


public class RtfShape extends RtfAddableElement {
    
    public static final int SHAPE_PICTURE_FRAME = 75;
    
    public static final int SHAPE_FREEFORM = 0;
    
    public static final int SHAPE_RECTANGLE = 1;
    
    public static final int SHAPE_ROUND_RECTANGLE = 2;
    
    public static final int SHAPE_ELLIPSE = 3;
    
    public static final int SHAPE_DIAMOND = 4;
    
    public static final int SHAPE_TRIANGLE_ISOSCELES = 5;
    
    public static final int SHAPE_TRIANGLE_RIGHT = 6;
    
    public static final int SHAPE_PARALLELOGRAM = 7;
    
    public static final int SHAPE_TRAPEZOID = 8;
    
    public static final int SHAPE_HEXAGON = 9;
    
    public static final int SHAPE_OCTAGON = 10;
    
    public static final int SHAPE_STAR = 12;
    
    public static final int SHAPE_ARROW = 13;
    
    public static final int SHAPE_ARROR_THICK = 14;
    
    public static final int SHAPE_HOME_PLATE = 15;
    
    public static final int SHAPE_CUBE = 16;
    
    public static final int SHAPE_BALLOON = 17;
    
    public static final int SHAPE_SEAL = 18;
    
    public static final int SHAPE_ARC = 19;
    
    public static final int SHAPE_LINE = 20;
    
    public static final int SHAPE_CAN = 22;
    
    public static final int SHAPE_DONUT = 23;
    
    
    public static final int SHAPE_WRAP_NONE = 0;
    
    public static final int SHAPE_WRAP_TOP_BOTTOM = 1;
    
    public static final int SHAPE_WRAP_BOTH = 2;
    
    public static final int SHAPE_WRAP_LEFT = 3;
    
    public static final int SHAPE_WRAP_RIGHT = 4;
    
    public static final int SHAPE_WRAP_LARGEST = 5;
    
    public static final int SHAPE_WRAP_TIGHT_BOTH = 6;
    
    public static final int SHAPE_WRAP_TIGHT_LEFT = 7;
    
    public static final int SHAPE_WRAP_TIGHT_RIGHT = 8;
    
    public static final int SHAPE_WRAP_TIGHT_LARGEST = 9;
    
    public static final int SHAPE_WRAP_THROUGH = 10;
    
    
    private int shapeNr = 0;
    
    private int type = 0;
    
    private RtfShapePosition position = null;
    
    private HashMap properties = null;
    
    private int wrapping = SHAPE_WRAP_NONE;
    
    private String shapeText = "";
    
    
    public RtfShape(int type, RtfShapePosition position) {
        this.type = type;
        this.position = position;
        this.properties = new HashMap();
    }

    
    public void setProperty(RtfShapeProperty property) {
        property.setRtfDocument(this.doc);
        this.properties.put(property.getName(), property);
    }
    
    
    public void setShapeText(String shapeText) {
        this.shapeText = shapeText;
    }

    
    public void setWrapping(int wrapping) {
        this.wrapping = wrapping;
    }

        
    public void writeContent(final OutputStream result) throws IOException
    {
        this.shapeNr = this.doc.getRandomInt();
        
        this.properties.put("ShapeType", new RtfShapeProperty("ShapeType", this.type));
        if(this.position.isShapeBelowText()) {
            this.properties.put("fBehindDocument", new RtfShapeProperty("fBehindDocument", true));
        }
        if(this.inTable) {
            this.properties.put("fLayoutInCell", new RtfShapeProperty("fLayoutInCell", true));
        }
        if(this.properties.containsKey("posh")) {
            this.position.setIgnoreXRelative(true);
        }
        if(this.properties.containsKey("posv")) {
            this.position.setIgnoreYRelative(true);
        }
        
        result.write(OPEN_GROUP);
        result.write(DocWriter.getISOBytes("\\shp"));
        result.write(DocWriter.getISOBytes("\\shplid"));
        result.write(intToByteArray(this.shapeNr));
        this.position.writeContent(result);
        switch(this.wrapping) {
        case SHAPE_WRAP_NONE:
            result.write(DocWriter.getISOBytes("\\shpwr3"));
            break;
        case SHAPE_WRAP_TOP_BOTTOM:
            result.write(DocWriter.getISOBytes("\\shpwr1"));
            break;
        case SHAPE_WRAP_BOTH:
            result.write(DocWriter.getISOBytes("\\shpwr2"));
            result.write(DocWriter.getISOBytes("\\shpwrk0"));
            break;
        case SHAPE_WRAP_LEFT:
            result.write(DocWriter.getISOBytes("\\shpwr2"));
            result.write(DocWriter.getISOBytes("\\shpwrk1"));
            break;
        case SHAPE_WRAP_RIGHT:
            result.write(DocWriter.getISOBytes("\\shpwr2"));
            result.write(DocWriter.getISOBytes("\\shpwrk2"));
            break;
        case SHAPE_WRAP_LARGEST:
            result.write(DocWriter.getISOBytes("\\shpwr2"));
            result.write(DocWriter.getISOBytes("\\shpwrk3"));
            break;
        case SHAPE_WRAP_TIGHT_BOTH:
            result.write(DocWriter.getISOBytes("\\shpwr4"));
            result.write(DocWriter.getISOBytes("\\shpwrk0"));
            break;
        case SHAPE_WRAP_TIGHT_LEFT:
            result.write(DocWriter.getISOBytes("\\shpwr4"));
            result.write(DocWriter.getISOBytes("\\shpwrk1"));
            break;
        case SHAPE_WRAP_TIGHT_RIGHT:
            result.write(DocWriter.getISOBytes("\\shpwr4"));
            result.write(DocWriter.getISOBytes("\\shpwrk2"));
            break;
        case SHAPE_WRAP_TIGHT_LARGEST:
            result.write(DocWriter.getISOBytes("\\shpwr4"));
            result.write(DocWriter.getISOBytes("\\shpwrk3"));
            break;
        case SHAPE_WRAP_THROUGH:
            result.write(DocWriter.getISOBytes("\\shpwr5"));
            break;
        default:
            result.write(DocWriter.getISOBytes("\\shpwr3"));
        }
        if(this.inHeader) {
            result.write(DocWriter.getISOBytes("\\shpfhdr1"));
        } 
        this.doc.outputDebugLinebreak(result);
        result.write(OPEN_GROUP);
        result.write(DocWriter.getISOBytes("\\*\\shpinst"));
        Iterator it = this.properties.values().iterator();
        while(it.hasNext()) {
            RtfShapeProperty rsp = (RtfShapeProperty) it.next();
            rsp.setRtfDocument(this.doc);
            rsp.writeContent(result);
        }
        if(!this.shapeText.equals("")) {
            result.write(OPEN_GROUP);
            result.write(DocWriter.getISOBytes("\\shptxt"));
            result.write(DELIMITER);
            result.write(DocWriter.getISOBytes(this.shapeText));
            result.write(CLOSE_GROUP);
        }
        result.write(CLOSE_GROUP);
        this.doc.outputDebugLinebreak(result);
        result.write(CLOSE_GROUP);
        
    }        
    
}
