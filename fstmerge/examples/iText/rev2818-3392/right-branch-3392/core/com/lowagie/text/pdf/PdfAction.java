

package com.lowagie.text.pdf;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;

import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.pdf.collection.PdfTargetDictionary;



public class PdfAction extends PdfDictionary {
    
    
    public static final int FIRSTPAGE = 1;
    
    public static final int PREVPAGE = 2;
    
    public static final int NEXTPAGE = 3;
    
    public static final int LASTPAGE = 4;

    
    public static final int PRINTDIALOG = 5;

    
    public static final int SUBMIT_EXCLUDE = 1;
    
    public static final int SUBMIT_INCLUDE_NO_VALUE_FIELDS = 2;
    
    public static final int SUBMIT_HTML_FORMAT = 4;
    
    public static final int SUBMIT_HTML_GET = 8;
    
    public static final int SUBMIT_COORDINATES = 16;
    
    public static final int SUBMIT_XFDF = 32;
    
    public static final int SUBMIT_INCLUDE_APPEND_SAVES = 64;
    
    public static final int SUBMIT_INCLUDE_ANNOTATIONS = 128;
    
    public static final int SUBMIT_PDF = 256;
    
    public static final int SUBMIT_CANONICAL_FORMAT = 512;
    
    public static final int SUBMIT_EXCL_NON_USER_ANNOTS = 1024;
    
    public static final int SUBMIT_EXCL_F_KEY = 2048;
    
    public static final int SUBMIT_EMBED_FORM = 8196;
    
    public static final int RESET_EXCLUDE = 1;

    
    
        
    public PdfAction() {
    }
    
    
    
    public PdfAction(URL url) {
        this(url.toExternalForm());
    }
    
    
    public PdfAction(URL url, boolean isMap) {
        this(url.toExternalForm(), isMap);
    }
    
    
    
    public PdfAction(String url) {
        this(url, false);
    }
    
    
    
    public PdfAction(String url, boolean isMap) {
        put(PdfName.S, PdfName.URI);
        put(PdfName.URI, new PdfString(url));
        if (isMap)
            put(PdfName.ISMAP, PdfBoolean.PDFTRUE);
    }
    
    
    
    PdfAction(PdfIndirectReference destination) {
        put(PdfName.S, PdfName.GOTO);
        put(PdfName.D, destination);
    }
    
    
    
    public PdfAction(String filename, String name) {
        put(PdfName.S, PdfName.GOTOR);
        put(PdfName.F, new PdfString(filename));
        put(PdfName.D, new PdfString(name));
    }
    
    
    
    public PdfAction(String filename, int page) {
        put(PdfName.S, PdfName.GOTOR);
        put(PdfName.F, new PdfString(filename));
        put(PdfName.D, new PdfLiteral("[" + (page - 1) + " /FitH 10000]"));
    }
    
    
    public PdfAction(int named) {
        put(PdfName.S, PdfName.NAMED);
        switch (named) {
            case FIRSTPAGE:
                put(PdfName.N, PdfName.FIRSTPAGE);
                break;
            case LASTPAGE:
                put(PdfName.N, PdfName.LASTPAGE);
                break;
            case NEXTPAGE:
                put(PdfName.N, PdfName.NEXTPAGE);
                break;
            case PREVPAGE:
                put(PdfName.N, PdfName.PREVPAGE);
                break;
            case PRINTDIALOG:
                put(PdfName.S, PdfName.JAVASCRIPT);
                put(PdfName.JS, new PdfString("this.print(true);\r"));
                break;
            default:
                throw new RuntimeException("Invalid named action.");
        }
    }
    
    
    public PdfAction(String application, String parameters, String operation, String defaultDir) {
        put(PdfName.S, PdfName.LAUNCH);
        if (parameters == null && operation == null && defaultDir == null)
            put(PdfName.F, new PdfString(application));
        else {
            PdfDictionary dic = new PdfDictionary();
            dic.put(PdfName.F, new PdfString(application));
            if (parameters != null)
                dic.put(PdfName.P, new PdfString(parameters));
            if (operation != null)
                dic.put(PdfName.O, new PdfString(operation));
            if (defaultDir != null)
                dic.put(PdfName.D, new PdfString(defaultDir));
            put(PdfName.WIN, dic);
        }
    }
    
    
    public static PdfAction createLaunch(String application, String parameters, String operation, String defaultDir) {
        return new PdfAction(application, parameters, operation, defaultDir);
    }
    
     
    public static PdfAction rendition(String file, PdfFileSpecification fs, String mimeType, PdfIndirectReference ref) throws IOException {
        PdfAction js = new PdfAction();
        js.put(PdfName.S, PdfName.RENDITION);
        js.put(PdfName.R, new PdfRendition(file, fs, mimeType));
        js.put(new PdfName("OP"), new PdfNumber(0));
        js.put(new PdfName("AN"), ref);
        return js;
     }
  
        
    public static PdfAction javaScript(String code, PdfWriter writer, boolean unicode) {
        PdfAction js = new PdfAction();
        js.put(PdfName.S, PdfName.JAVASCRIPT);
        if (unicode && code.length() < 50) {
                js.put(PdfName.JS, new PdfString(code, PdfObject.TEXT_UNICODE));
        }
        else if (!unicode && code.length() < 100) {
                js.put(PdfName.JS, new PdfString(code));
        }
        else {
            try {
                byte b[] = PdfEncodings.convertToBytes(code, unicode ? PdfObject.TEXT_UNICODE : PdfObject.TEXT_PDFDOCENCODING);
                PdfStream stream = new PdfStream(b);
                stream.flateCompress();
                js.put(PdfName.JS, writer.addToBody(stream).getIndirectReference());
            }
            catch (Exception e) {
                throw new ExceptionConverter(e);
            }
        }
        return js;
    }

        
    public static PdfAction javaScript(String code, PdfWriter writer) {
        return javaScript(code, writer, false);
    }
    
    
    static PdfAction createHide(PdfObject obj, boolean hide) {
        PdfAction action = new PdfAction();
        action.put(PdfName.S, PdfName.HIDE);
        action.put(PdfName.T, obj);
        if (!hide)
            action.put(PdfName.H, PdfBoolean.PDFFALSE);
        return action;
    }
    
    
    public static PdfAction createHide(PdfAnnotation annot, boolean hide) {
        return createHide(annot.getIndirectReference(), hide);
    }
    
    
    public static PdfAction createHide(String name, boolean hide) {
        return createHide(new PdfString(name), hide);
    }
    
    static PdfArray buildArray(Object names[]) {
        PdfArray array = new PdfArray();
        for (int k = 0; k < names.length; ++k) {
            Object obj = names[k];
            if (obj instanceof String)
                array.add(new PdfString((String)obj));
            else if (obj instanceof PdfAnnotation)
                array.add(((PdfAnnotation)obj).getIndirectReference());
            else
                throw new RuntimeException("The array must contain String or PdfAnnotation.");
        }
        return array;
    }
    
    
    public static PdfAction createHide(Object names[], boolean hide) {
        return createHide(buildArray(names), hide);
    }
    
    
    public static PdfAction createSubmitForm(String file, Object names[], int flags) {
        PdfAction action = new PdfAction();
        action.put(PdfName.S, PdfName.SUBMITFORM);
        PdfDictionary dic = new PdfDictionary();
        dic.put(PdfName.F, new PdfString(file));
        dic.put(PdfName.FS, PdfName.URL);
        action.put(PdfName.F, dic);
        if (names != null)
            action.put(PdfName.FIELDS, buildArray(names));
        action.put(PdfName.FLAGS, new PdfNumber(flags));
        return action;
    }
    
    
    public static PdfAction createResetForm(Object names[], int flags) {
        PdfAction action = new PdfAction();
        action.put(PdfName.S, PdfName.RESETFORM);
        if (names != null)
            action.put(PdfName.FIELDS, buildArray(names));
        action.put(PdfName.FLAGS, new PdfNumber(flags));
        return action;
    }
    
    
    public static PdfAction createImportData(String file) {
        PdfAction action = new PdfAction();
        action.put(PdfName.S, PdfName.IMPORTDATA);
        action.put(PdfName.F, new PdfString(file));
        return action;
    }
    
        
    public void next(PdfAction na) {
        PdfObject nextAction = get(PdfName.NEXT);
        if (nextAction == null)
            put(PdfName.NEXT, na);
        else if (nextAction.isDictionary()) {
            PdfArray array = new PdfArray(nextAction);
            array.add(na);
            put(PdfName.NEXT, array);
        }
        else {
            ((PdfArray)nextAction).add(na);
        }
    }
    
        
    public static PdfAction gotoLocalPage(int page, PdfDestination dest, PdfWriter writer) {
        PdfIndirectReference ref = writer.getPageReference(page);
        dest.addPage(ref);
        PdfAction action = new PdfAction();
        action.put(PdfName.S, PdfName.GOTO);
        action.put(PdfName.D, dest);
        return action;
    }

    
    public static PdfAction gotoLocalPage(String dest, boolean isName) {
        PdfAction action = new PdfAction();
        action.put(PdfName.S, PdfName.GOTO);
        if (isName)
            action.put(PdfName.D, new PdfName(dest));
        else
            action.put(PdfName.D, new PdfString(dest, null));
        return action;
    }

    
    public static PdfAction gotoRemotePage(String filename, String dest, boolean isName, boolean newWindow) {
        PdfAction action = new PdfAction();
        action.put(PdfName.F, new PdfString(filename));
        action.put(PdfName.S, PdfName.GOTOR);
        if (isName)
            action.put(PdfName.D, new PdfName(dest));
        else
            action.put(PdfName.D, new PdfString(dest, null));
        if (newWindow)
            action.put(PdfName.NEWWINDOW, PdfBoolean.PDFTRUE);
        return action;
    }

    
    public static PdfAction gotoEmbedded(String filename, PdfTargetDictionary target, String dest, boolean isName, boolean newWindow) {
        if (isName)
            return gotoEmbedded(filename, target, new PdfName(dest), newWindow);
        else
            return gotoEmbedded(filename, target, new PdfString(dest, null), newWindow);
    }

    
    public static PdfAction gotoEmbedded(String filename, PdfTargetDictionary target, PdfObject dest, boolean newWindow) {
        PdfAction action = new PdfAction();
        action.put(PdfName.S, PdfName.GOTOE);
        action.put(PdfName.T, target);
        action.put(PdfName.D, dest);
        action.put(PdfName.NEWWINDOW, new PdfBoolean(newWindow));
        if (filename != null) {
            action.put(PdfName.F, new PdfString(filename));
        }
        return action;
    }

        
    public static PdfAction setOCGstate(ArrayList<Object> state, boolean preserveRB) {
        PdfAction action = new PdfAction();
        action.put(PdfName.S, PdfName.SETOCGSTATE);
        PdfArray a = new PdfArray();
        for (int k = 0; k < state.size(); ++k) {
            Object o = state.get(k);
            if (o == null)
                continue;
            if (o instanceof PdfIndirectReference)
                a.add((PdfIndirectReference)o);
            else if (o instanceof PdfLayer)
                a.add(((PdfLayer)o).getRef());
            else if (o instanceof PdfName)
                a.add((PdfName)o);
            else if (o instanceof String) {
                PdfName name = null;
                String s = (String)o;
                if (s.equalsIgnoreCase("on"))
                    name = PdfName.ON;
                else if (s.equalsIgnoreCase("off"))
                    name = PdfName.OFF;
                else if (s.equalsIgnoreCase("toggle"))
                    name = PdfName.TOGGLE;
                else
                    throw new IllegalArgumentException("A string '" + s + " was passed in state. Only 'ON', 'OFF' and 'Toggle' are allowed.");
                a.add(name);
            }
            else
                throw new IllegalArgumentException("Invalid type was passed in state: " + o.getClass().getName());
        }
        action.put(PdfName.STATE, a);
        if (!preserveRB)
            action.put(PdfName.PRESERVERB, PdfBoolean.PDFFALSE);
        return action;
    }
}
