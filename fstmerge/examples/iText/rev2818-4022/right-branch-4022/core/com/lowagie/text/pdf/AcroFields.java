
package com.lowagie.text.pdf;

import java.awt.Color;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.w3c.dom.Node;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.codec.Base64;


public class AcroFields {

    PdfReader reader;
    PdfWriter writer;
    HashMap<String, Item> fields;
    private int topFirst;
    private HashMap<String, int[]> sigNames;
    private boolean append;
    public static final int DA_FONT = 0;
    public static final int DA_SIZE = 1;
    public static final int DA_COLOR = 2;
    private HashMap<Integer, BaseFont> extensionFonts = new HashMap<Integer, BaseFont>();
    private XfaForm xfa;
    
    
    public static final int FIELD_TYPE_NONE = 0;
    
    
    public static final int FIELD_TYPE_PUSHBUTTON = 1;
    
    
    public static final int FIELD_TYPE_CHECKBOX = 2;
    
    
    public static final int FIELD_TYPE_RADIOBUTTON = 3;
    
    
    public static final int FIELD_TYPE_TEXT = 4;
    
    
    public static final int FIELD_TYPE_LIST = 5;
    
    
    public static final int FIELD_TYPE_COMBO = 6;
    
    
    public static final int FIELD_TYPE_SIGNATURE = 7;

    private boolean lastWasString;

    
    private boolean generateAppearances = true;

    private HashMap<String, BaseFont> localFonts = new HashMap<String, BaseFont>();

    private float extraMarginLeft;
    private float extraMarginTop;
    private ArrayList<BaseFont> substitutionFonts;

    AcroFields(PdfReader reader, PdfWriter writer) {
        this.reader = reader;
        this.writer = writer;
        try {
            xfa = new XfaForm(reader);
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
        if (writer instanceof PdfStamperImp) {
            append = ((PdfStamperImp)writer).isAppend();
        }
        fill();
    }

    void fill() {
        fields = new HashMap<String, Item>();
        PdfDictionary top = (PdfDictionary)PdfReader.getPdfObjectRelease(reader.getCatalog().get(PdfName.ACROFORM));
        if (top == null)
            return;
        PdfArray arrfds = (PdfArray)PdfReader.getPdfObjectRelease(top.get(PdfName.FIELDS));
        if (arrfds == null || arrfds.size() == 0)
            return;
        for (int k = 1; k <= reader.getNumberOfPages(); ++k) {
            PdfDictionary page = reader.getPageNRelease(k);
            PdfArray annots = (PdfArray)PdfReader.getPdfObjectRelease(page.get(PdfName.ANNOTS), page);
            if (annots == null)
                continue;
            for (int j = 0; j < annots.size(); ++j) {
                PdfDictionary annot = annots.getAsDict(j);
                if (annot == null) {
                    PdfReader.releaseLastXrefPartial(annots.getAsIndirectObject(j));
                    continue;
                }
                if (!PdfName.WIDGET.equals(annot.getAsName(PdfName.SUBTYPE))) {
                    PdfReader.releaseLastXrefPartial(annots.getAsIndirectObject(j));
                    continue;
                }
                PdfDictionary widget = annot;
                PdfDictionary dic = new PdfDictionary();
                dic.putAll(annot);
                String name = "";
                PdfDictionary value = null;
                PdfObject lastV = null;
                while (annot != null) {
                    dic.mergeDifferent(annot);
                    PdfString t = annot.getAsString(PdfName.T);
                    if (t != null)
                        name = t.toUnicodeString() + "." + name;
                    if (lastV == null && annot.get(PdfName.V) != null)
                        lastV = PdfReader.getPdfObjectRelease(annot.get(PdfName.V));
                    if (value == null &&  t != null) {
                        value = annot;
                        if (annot.get(PdfName.V) == null && lastV  != null)
                            value.put(PdfName.V, lastV);
                    }
                    annot = annot.getAsDict(PdfName.PARENT);
                }
                if (name.length() > 0)
                    name = name.substring(0, name.length() - 1);
                Item item = fields.get(name);
                if (item == null) {
                    item = new Item();
                    fields.put(name, item);
                }
                if (value == null)
                    item.addValue(widget);
                else
                    item.addValue(value);
                item.addWidget(widget);
                item.addWidgetRef(annots.getAsIndirectObject(j)); 
                if (top != null)
                    dic.mergeDifferent(top);
                item.addMerged(dic);
                item.addPage(k);
                item.addTabOrder(j);
            }
        }
        
        
        PdfNumber sigFlags = top.getAsNumber(PdfName.SIGFLAGS);
        if (sigFlags == null || (sigFlags.intValue() & 1) != 1)
            return;
        for (int j = 0; j < arrfds.size(); ++j) {
            PdfDictionary annot = arrfds.getAsDict(j);
            if (annot == null) {
                PdfReader.releaseLastXrefPartial(arrfds.getAsIndirectObject(j));
                continue;
            }
            if (!PdfName.WIDGET.equals(annot.getAsName(PdfName.SUBTYPE))) {
                PdfReader.releaseLastXrefPartial(arrfds.getAsIndirectObject(j));
                continue;
            }
            PdfArray kids = (PdfArray)PdfReader.getPdfObjectRelease(annot.get(PdfName.KIDS));
            if (kids != null)
                continue;
            PdfDictionary dic = new PdfDictionary();
            dic.putAll(annot);
            PdfString t = annot.getAsString(PdfName.T);
            if (t == null)
                continue;
            String name = t.toUnicodeString();
            if (fields.containsKey(name))
                continue;
            Item item = new Item();
            fields.put(name, item);
            item.addValue(dic);
            item.addWidget(dic);
            item.addWidgetRef(arrfds.getAsIndirectObject(j)); 
            item.addMerged(dic);
            item.addPage(-1);
            item.addTabOrder(-1);
        }
    }

    
    public String[] getAppearanceStates(String fieldName) {
        Item fd = fields.get(fieldName);
        if (fd == null)
            return null;
        HashSet<String> names = new HashSet<String>();
        PdfDictionary vals = fd.getValue(0);
        PdfString stringOpt = vals.getAsString( PdfName.OPT );
        if (stringOpt != null) {
            names.add(stringOpt.toUnicodeString());
        }
        else {
            PdfArray arrayOpt = vals.getAsArray(PdfName.OPT);
            if (arrayOpt != null) {
                for (int k = 0; k < arrayOpt.size(); ++k) {
                    PdfString valStr = arrayOpt.getAsString( k );
                    if (valStr != null)
                        names.add(valStr.toUnicodeString());
                }
            }
        }
        for (int k = 0; k < fd.size(); ++k) {
            PdfDictionary dic = fd.getWidget( k );
            dic = dic.getAsDict(PdfName.AP);
            if (dic == null)
                continue;
            dic = dic.getAsDict(PdfName.N);
            if (dic == null)
                continue;
            for (PdfName n: dic.getKeys()) {
                String name = PdfName.decodeName(n.toString());
                names.add(name);
            }
        }
        String out[] = new String[names.size()];
        return names.toArray(out);
    }

    private String[] getListOption(String fieldName, int idx) {
        Item fd = getFieldItem(fieldName);
        if (fd == null)
            return null;
        PdfArray ar = fd.getMerged(0).getAsArray(PdfName.OPT);
        if (ar == null)
            return null;
        String[] ret = new String[ar.size()];
        for (int k = 0; k < ar.size(); ++k) {
            PdfObject obj = ar.getDirectObject( k );
            try {
                if (obj.isArray()) {
                    obj = ((PdfArray)obj).getDirectObject(idx);
                }
                if (obj.isString())
                    ret[k] = ((PdfString)obj).toUnicodeString();
                else
                    ret[k] = obj.toString();
            }
            catch (Exception e) {
                ret[k] = "";
            }
        }
        return ret;
    }

    
    public String[] getListOptionExport(String fieldName) {
        return getListOption(fieldName, 0);
    }

    
    public String[] getListOptionDisplay(String fieldName) {
        return getListOption(fieldName, 1);
    }

    
    public boolean setListOption(String fieldName, String[] exportValues, String[] displayValues) {
        if (exportValues == null && displayValues == null)
            return false;
        if (exportValues != null && displayValues != null && exportValues.length != displayValues.length)
            throw new IllegalArgumentException("The export and the display array must have the same size.");
        int ftype = getFieldType(fieldName);
        if (ftype != FIELD_TYPE_COMBO && ftype != FIELD_TYPE_LIST)
            return false;
        Item fd = fields.get(fieldName);
        String[] sing = null;
        if (exportValues == null && displayValues != null)
            sing = displayValues;
        else if (exportValues != null && displayValues == null)
            sing = exportValues;
        PdfArray opt = new PdfArray();
        if (sing != null) {
            for (int k = 0; k < sing.length; ++k)
                opt.add(new PdfString(sing[k], PdfObject.TEXT_UNICODE));
        }
        else {
            for (int k = 0; k < exportValues.length; ++k) {
                PdfArray a = new PdfArray();
                a.add(new PdfString(exportValues[k], PdfObject.TEXT_UNICODE));
                a.add(new PdfString(displayValues[k], PdfObject.TEXT_UNICODE));
                opt.add(a);
            }
        }
        fd.writeToAll( PdfName.OPT, opt, Item.WRITE_VALUE | Item.WRITE_MERGED );
        return true;
    }

    
    public int getFieldType(String fieldName) {
        Item fd = getFieldItem(fieldName);
        if (fd == null)
            return FIELD_TYPE_NONE;
        PdfDictionary merged = fd.getMerged( 0 );
        PdfName type = merged.getAsName(PdfName.FT);
        if (type == null)
            return FIELD_TYPE_NONE;
        int ff = 0;
        PdfNumber ffo = merged.getAsNumber(PdfName.FF);
        if (ffo != null) {
            ff = ffo.intValue();
        }
        if (PdfName.BTN.equals(type)) {
            if ((ff & PdfFormField.FF_PUSHBUTTON) != 0)
                return FIELD_TYPE_PUSHBUTTON;
            if ((ff & PdfFormField.FF_RADIO) != 0)
                return FIELD_TYPE_RADIOBUTTON;
            else
                return FIELD_TYPE_CHECKBOX;
        }
        else if (PdfName.TX.equals(type)) {
            return FIELD_TYPE_TEXT;
        }
        else if (PdfName.CH.equals(type)) {
            if ((ff & PdfFormField.FF_COMBO) != 0)
                return FIELD_TYPE_COMBO;
            else
                return FIELD_TYPE_LIST;
        }
        else if (PdfName.SIG.equals(type)) {
            return FIELD_TYPE_SIGNATURE;
        }
        return FIELD_TYPE_NONE;
    }

    
    public void exportAsFdf(FdfWriter writer) {
        for (Map.Entry<String, Item> entry: fields.entrySet()) {
            Item item = entry.getValue();
            String name = entry.getKey();
            PdfObject v = item.getMerged(0).get(PdfName.V);
            if (v == null)
                continue;
            String value = getField(name);
            if (lastWasString)
                writer.setFieldAsString(name, value);
            else
                writer.setFieldAsName(name, value);
        }
    }

    
    public boolean renameField(String oldName, String newName) {
        int idx1 = oldName.lastIndexOf('.') + 1;
        int idx2 = newName.lastIndexOf('.') + 1;
        if (idx1 != idx2)
            return false;
        if (!oldName.substring(0, idx1).equals(newName.substring(0, idx2)))
            return false;
        if (fields.containsKey(newName))
            return false;
        Item item = fields.get(oldName);
        if (item == null)
            return false;
        newName = newName.substring(idx2);
        PdfString ss = new PdfString(newName, PdfObject.TEXT_UNICODE);

        item.writeToAll( PdfName.T, ss, Item.WRITE_VALUE | Item.WRITE_MERGED);
        item.markUsed( this, Item.WRITE_VALUE );

        fields.remove(oldName);
        fields.put(newName, item);

        return true;
    }

    public static Object[] splitDAelements(String da) {
        try {
            PRTokeniser tk = new PRTokeniser(PdfEncodings.convertToBytes(da, null));
            ArrayList<String> stack = new ArrayList<String>();
            Object ret[] = new Object[3];
            while (tk.nextToken()) {
                if (tk.getTokenType() == PRTokeniser.TK_COMMENT)
                    continue;
                if (tk.getTokenType() == PRTokeniser.TK_OTHER) {
                    String operator = tk.getStringValue();
                    if (operator.equals("Tf")) {
                        if (stack.size() >= 2) {
                            ret[DA_FONT] = stack.get(stack.size() - 2);
                            ret[DA_SIZE] = new Float(stack.get(stack.size() - 1));
                        }
                    }
                    else if (operator.equals("g")) {
                        if (stack.size() >= 1) {
                            float gray = new Float(stack.get(stack.size() - 1)).floatValue();
                            if (gray != 0)
                                ret[DA_COLOR] = new GrayColor(gray);
                        }
                    }
                    else if (operator.equals("rg")) {
                        if (stack.size() >= 3) {
                            float red = new Float(stack.get(stack.size() - 3)).floatValue();
                            float green = new Float(stack.get(stack.size() - 2)).floatValue();
                            float blue = new Float(stack.get(stack.size() - 1)).floatValue();
                            ret[DA_COLOR] = new Color(red, green, blue);
                        }
                    }
                    else if (operator.equals("k")) {
                        if (stack.size() >= 4) {
                            float cyan = new Float(stack.get(stack.size() - 4)).floatValue();
                            float magenta = new Float(stack.get(stack.size() - 3)).floatValue();
                            float yellow = new Float(stack.get(stack.size() - 2)).floatValue();
                            float black = new Float(stack.get(stack.size() - 1)).floatValue();
                            ret[DA_COLOR] = new CMYKColor(cyan, magenta, yellow, black);
                        }
                    }
                    stack.clear();
                }
                else
                    stack.add(tk.getStringValue());
            }
            return ret;
        }
        catch (IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }

    public void decodeGenericDictionary(PdfDictionary merged, BaseField tx) throws IOException, DocumentException {
        int flags = 0;
        
        PdfString da = merged.getAsString(PdfName.DA);
        if (da != null) {
            Object dab[] = splitDAelements(da.toUnicodeString());
            if (dab[DA_SIZE] != null)
                tx.setFontSize(((Float)dab[DA_SIZE]).floatValue());
            if (dab[DA_COLOR] != null)
                tx.setTextColor((Color)dab[DA_COLOR]);
            if (dab[DA_FONT] != null) {
                PdfDictionary font = merged.getAsDict(PdfName.DR);
                if (font != null) {
                    font = font.getAsDict(PdfName.FONT);
                    if (font != null) {
                        PdfObject po = font.get(new PdfName((String)dab[DA_FONT]));
                        if (po != null && po.type() == PdfObject.INDIRECT) {
                            PRIndirectReference por = (PRIndirectReference)po;
                            BaseFont bp = new DocumentFont((PRIndirectReference)po);
                            tx.setFont(bp);
                            Integer porkey = new Integer(por.getNumber());
                            BaseFont porf = extensionFonts.get(porkey);
                            if (porf == null) {
                                if (!extensionFonts.containsKey(porkey)) {
                                    PdfDictionary fo = (PdfDictionary)PdfReader.getPdfObject(po);
                                    PdfDictionary fd = fo.getAsDict(PdfName.FONTDESCRIPTOR);
                                    if (fd != null) {
                                        PRStream prs = (PRStream)PdfReader.getPdfObject(fd.get(PdfName.FONTFILE2));
                                        if (prs == null)
                                            prs = (PRStream)PdfReader.getPdfObject(fd.get(PdfName.FONTFILE3));
                                        if (prs == null) {
                                            extensionFonts.put(porkey, null);
                                        }
                                        else {
                                            try {
                                                porf = BaseFont.createFont("font.ttf", BaseFont.IDENTITY_H, true, false, PdfReader.getStreamBytes(prs), null);
                                            }
                                            catch (Exception e) {
                                            }
                                            extensionFonts.put(porkey, porf);
                                        }
                                    }
                                }
                            }
                            if (tx instanceof TextField)
                                ((TextField)tx).setExtensionFont(porf);
                        }
                        else {
                            BaseFont bf = localFonts.get(dab[DA_FONT]);
                            if (bf == null) {
                                String fn[] = stdFieldFontNames.get(dab[DA_FONT]);
                                if (fn != null) {
                                    try {
                                        String enc = "winansi";
                                        if (fn.length > 1)
                                            enc = fn[1];
                                        bf = BaseFont.createFont(fn[0], enc, false);
                                        tx.setFont(bf);
                                    }
                                    catch (Exception e) {
                                        
                                    }
                                }
                            }
                            else
                                tx.setFont(bf);
                        }
                    }
                }
            }
        }
        
        PdfDictionary mk = merged.getAsDict(PdfName.MK);
        if (mk != null) {
            PdfArray ar = mk.getAsArray(PdfName.BC);
            Color border = getMKColor(ar);
            tx.setBorderColor(border);
            if (border != null)
                tx.setBorderWidth(1);
            ar = mk.getAsArray(PdfName.BG);
            tx.setBackgroundColor(getMKColor(ar));
            PdfNumber rotation = mk.getAsNumber(PdfName.R);
            if (rotation != null)
                tx.setRotation(rotation.intValue());
        }
        
        PdfNumber nfl = merged.getAsNumber(PdfName.F);
        flags = 0;
        tx.setVisibility(BaseField.VISIBLE_BUT_DOES_NOT_PRINT);
        if (nfl != null) {
            flags = nfl.intValue();
            if ((flags & PdfFormField.FLAGS_PRINT) != 0 && (flags & PdfFormField.FLAGS_HIDDEN) != 0)
                tx.setVisibility(BaseField.HIDDEN);
            else if ((flags & PdfFormField.FLAGS_PRINT) != 0 && (flags & PdfFormField.FLAGS_NOVIEW) != 0)
                tx.setVisibility(BaseField.HIDDEN_BUT_PRINTABLE);
            else if ((flags & PdfFormField.FLAGS_PRINT) != 0)
                tx.setVisibility(BaseField.VISIBLE);
        }
        
        nfl = merged.getAsNumber(PdfName.FF);
        flags = 0;
        if (nfl != null)
            flags = nfl.intValue();
        tx.setOptions(flags);
        if ((flags & PdfFormField.FF_COMB) != 0) {
            PdfNumber maxLen = merged.getAsNumber(PdfName.MAXLEN);
            int len = 0;
            if (maxLen != null)
                len = maxLen.intValue();
            tx.setMaxCharacterLength(len);
        }
        
        nfl = merged.getAsNumber(PdfName.Q);
        if (nfl != null) {
            if (nfl.intValue() == PdfFormField.Q_CENTER)
                tx.setAlignment(Element.ALIGN_CENTER);
            else if (nfl.intValue() == PdfFormField.Q_RIGHT)
                tx.setAlignment(Element.ALIGN_RIGHT);
        }
        
        PdfDictionary bs = merged.getAsDict(PdfName.BS);
        if (bs != null) {
            PdfNumber w = bs.getAsNumber(PdfName.W);
            if (w != null)
                tx.setBorderWidth(w.floatValue());
            PdfName s = bs.getAsName(PdfName.S);
            if (PdfName.D.equals(s))
                tx.setBorderStyle(PdfBorderDictionary.STYLE_DASHED);
            else if (PdfName.B.equals(s))
                tx.setBorderStyle(PdfBorderDictionary.STYLE_BEVELED);
            else if (PdfName.I.equals(s))
                tx.setBorderStyle(PdfBorderDictionary.STYLE_INSET);
            else if (PdfName.U.equals(s))
                tx.setBorderStyle(PdfBorderDictionary.STYLE_UNDERLINE);
        }
        else {
            PdfArray bd = merged.getAsArray(PdfName.BORDER);
            if (bd != null) {
                if (bd.size() >= 3)
                    tx.setBorderWidth(bd.getAsNumber(2).floatValue());
                if (bd.size() >= 4)
                    tx.setBorderStyle(PdfBorderDictionary.STYLE_DASHED);
            }
        }
    }

    PdfAppearance getAppearance(PdfDictionary merged, String text, String fieldName) throws IOException, DocumentException {
        topFirst = 0;
        TextField tx = null;
        if (fieldCache == null || !fieldCache.containsKey(fieldName)) {
            tx = new TextField(writer, null, null);
            tx.setExtraMargin(extraMarginLeft, extraMarginTop);
            tx.setBorderWidth(0);
            tx.setSubstitutionFonts(substitutionFonts);
            decodeGenericDictionary(merged, tx);
            
            PdfArray rect = merged.getAsArray(PdfName.RECT);
            Rectangle box = PdfReader.getNormalizedRectangle(rect);
            if (tx.getRotation() == 90 || tx.getRotation() == 270)
                box = box.rotate();
            tx.setBox(box);
            if (fieldCache != null)
                fieldCache.put(fieldName, tx);
        }
        else {
            tx = fieldCache.get(fieldName);
            tx.setWriter(writer);
        }
        PdfName fieldType = merged.getAsName(PdfName.FT);
        if (PdfName.TX.equals(fieldType)) {
            tx.setText(text);
            return tx.getAppearance();
        }
        if (!PdfName.CH.equals(fieldType))
            throw new DocumentException("An appearance was requested without a variable text field.");
        PdfArray opt = merged.getAsArray(PdfName.OPT);
        int flags = 0;
        PdfNumber nfl = merged.getAsNumber(PdfName.FF);
        if (nfl != null)
            flags = nfl.intValue();
        if ((flags & PdfFormField.FF_COMBO) != 0 && opt == null) {
            tx.setText(text);
            return tx.getAppearance();
        }
        if (opt != null) {
            String choices[] = new String[opt.size()];
            String choicesExp[] = new String[opt.size()];
            for (int k = 0; k < opt.size(); ++k) {
                PdfObject obj = opt.getPdfObject(k);
                if (obj.isString()) {
                    choices[k] = choicesExp[k] = ((PdfString)obj).toUnicodeString();
                }
                else {
                    PdfArray a = (PdfArray) obj;
                    choicesExp[k] = a.getAsString(0).toUnicodeString();
                    choices[k] = a.getAsString(1).toUnicodeString();
                }
            }
            if ((flags & PdfFormField.FF_COMBO) != 0) {
                for (int k = 0; k < choices.length; ++k) {
                    if (text.equals(choicesExp[k])) {
                        text = choices[k];
                        break;
                    }
                }
                tx.setText(text);
                return tx.getAppearance();
            }
            int idx = 0;
            for (int k = 0; k < choicesExp.length; ++k) {
                if (text.equals(choicesExp[k])) {
                    idx = k;
                    break;
                }
            }
            tx.setChoices(choices);
            tx.setChoiceExports(choicesExp);
            tx.setChoiceSelection(idx);
        }
        PdfAppearance app = tx.getListAppearance();
        topFirst = tx.getTopFirst();
        return app;
    }

    Color getMKColor(PdfArray ar) {
        if (ar == null)
            return null;
        switch (ar.size()) {
            case 1:
                return new GrayColor(ar.getAsNumber(0).floatValue());
            case 3:
                return new Color(ExtendedColor.normalize(ar.getAsNumber(0).floatValue()), ExtendedColor.normalize(ar.getAsNumber(1).floatValue()), ExtendedColor.normalize(ar.getAsNumber(2).floatValue()));
            case 4:
                return new CMYKColor(ar.getAsNumber(0).floatValue(), ar.getAsNumber(1).floatValue(), ar.getAsNumber(2).floatValue(), ar.getAsNumber(3).floatValue());
            default:
                return null;
        }
    }

    
    public String getField(String name) {
        if (xfa.isXfaPresent()) {
            name = xfa.findFieldName(name, this);
            if (name == null)
                return null;
            name = XfaForm.Xml2Som.getShortName(name);
            return XfaForm.getNodeText(xfa.findDatasetsNode(name));
        }
        Item item = fields.get(name);
        if (item == null)
            return null;
        lastWasString = false;
        PdfDictionary mergedDict = item.getMerged( 0 );

        
        
        
        PdfObject v = PdfReader.getPdfObject(mergedDict.get(PdfName.V));
        if (v == null)
            return "";
        if (v instanceof PRStream) {
                byte[] valBytes;
                try {
                    valBytes = PdfReader.getStreamBytes((PRStream)v);
                    return new String(valBytes);
                } catch (IOException e) {
                    throw new ExceptionConverter(e);
                }
        }
        
        PdfName type = mergedDict.getAsName(PdfName.FT);
        if (PdfName.BTN.equals(type)) {
            PdfNumber ff = mergedDict.getAsNumber(PdfName.FF);
            int flags = 0;
            if (ff != null)
                flags = ff.intValue();
            if ((flags & PdfFormField.FF_PUSHBUTTON) != 0)
                return "";
            String value = "";
            if (v instanceof PdfName)
                value = PdfName.decodeName(v.toString());
            else if (v instanceof PdfString)
                value = ((PdfString)v).toUnicodeString();
            PdfArray opts = item.getValue(0).getAsArray(PdfName.OPT);
            if (opts != null) {
                int idx = 0;
                try {
                    idx = Integer.parseInt(value);
                    PdfString ps = opts.getAsString(idx);
                    value = ps.toUnicodeString();
                    lastWasString = true;
                }
                catch (Exception e) {
                }
            }
            return value;
        }
        if (v instanceof PdfString) {
            lastWasString = true;
            return ((PdfString)v).toUnicodeString();
        } else if (v instanceof PdfName) {
            return PdfName.decodeName(v.toString());
        } else
            return "";
    }

    
    public String[] getListSelection(String name) {
        String[] ret;
        String s = getField(name);
        if (s == null) {
            ret = new String[]{};
        }
        else {
            ret = new String[]{ s };
        }
        Item item = fields.get(name);
        if (item == null)
            return ret;
        
        
        
        
        PdfArray values = item.getMerged(0).getAsArray(PdfName.I);
        if (values == null)
            return ret;
        ret = new String[values.size()];
        String[] options = getListOptionExport(name);
        PdfNumber n;
        int idx = 0;
        for (Iterator<PdfObject> i = values.listIterator(); i.hasNext(); ) {
            n = (PdfNumber)i.next();
            ret[idx++] = options[n.intValue()];
        }
        return ret;
    }


    
    public boolean setFieldProperty(String field, String name, Object value, int inst[]) {
        if (writer == null)
            throw new RuntimeException("This AcroFields instance is read-only.");
        try {
            Item item = fields.get(field);
            if (item == null)
                return false;
            InstHit hit = new InstHit(inst);
            PdfDictionary merged;
            PdfString da;
            if (name.equalsIgnoreCase("textfont")) {
                for (int k = 0; k < item.size(); ++k) {
                    if (hit.isHit(k)) {
                        merged = item.getMerged( k );
                        da = merged.getAsString(PdfName.DA);
                        PdfDictionary dr = merged.getAsDict(PdfName.DR);
                        if (da != null && dr != null) {
                            Object dao[] = splitDAelements(da.toUnicodeString());
                            PdfAppearance cb = new PdfAppearance();
                            if (dao[DA_FONT] != null) {
                                BaseFont bf = (BaseFont)value;
                                PdfName psn = PdfAppearance.stdFieldFontNames.get(bf.getPostscriptFontName());
                                if (psn == null) {
                                    psn = new PdfName(bf.getPostscriptFontName());
                                }
                                PdfDictionary fonts = dr.getAsDict(PdfName.FONT);
                                if (fonts == null) {
                                    fonts = new PdfDictionary();
                                    dr.put(PdfName.FONT, fonts);
                                }
                                PdfIndirectReference fref = (PdfIndirectReference)fonts.get(psn);
                                PdfDictionary top = reader.getCatalog().getAsDict(PdfName.ACROFORM);
                                markUsed(top);
                                dr = top.getAsDict(PdfName.DR);
                                if (dr == null) {
                                    dr = new PdfDictionary();
                                    top.put(PdfName.DR, dr);
                                }
                                markUsed(dr);
                                PdfDictionary fontsTop = dr.getAsDict(PdfName.FONT);
                                if (fontsTop == null) {
                                    fontsTop = new PdfDictionary();
                                    dr.put(PdfName.FONT, fontsTop);
                                }
                                markUsed(fontsTop);
                                PdfIndirectReference frefTop = (PdfIndirectReference)fontsTop.get(psn);
                                if (frefTop != null) {
                                    if (fref == null)
                                        fonts.put(psn, frefTop);
                                }
                                else if (fref == null) {
                                    FontDetails fd;
                                    if (bf.getFontType() == BaseFont.FONT_TYPE_DOCUMENT) {
                                        fd = new FontDetails(null, ((DocumentFont)bf).getIndirectReference(), bf);
                                    }
                                    else {
                                        bf.setSubset(false);
                                        fd = writer.addSimple(bf);
                                        localFonts.put(psn.toString().substring(1), bf);
                                    }
                                    fontsTop.put(psn, fd.getIndirectReference());
                                    fonts.put(psn, fd.getIndirectReference());
                                }
                                ByteBuffer buf = cb.getInternalBuffer();
                                buf.append(psn.getBytes()).append(' ').append(((Float)dao[DA_SIZE]).floatValue()).append(" Tf ");
                                if (dao[DA_COLOR] != null)
                                    cb.setColorFill((Color)dao[DA_COLOR]);
                                PdfString s = new PdfString(cb.toString());
                                item.getMerged(k).put(PdfName.DA, s);
                                item.getWidget(k).put(PdfName.DA, s);
                                markUsed(item.getWidget(k));
                            }
                        }
                    }
                }
            }
            else if (name.equalsIgnoreCase("textcolor")) {
                for (int k = 0; k < item.size(); ++k) {
                    if (hit.isHit(k)) {
                        merged = item.getMerged( k );
                        da = merged.getAsString(PdfName.DA);
                        if (da != null) {
                            Object dao[] = splitDAelements(da.toUnicodeString());
                            PdfAppearance cb = new PdfAppearance();
                            if (dao[DA_FONT] != null) {
                                ByteBuffer buf = cb.getInternalBuffer();
                                buf.append(new PdfName((String)dao[DA_FONT]).getBytes()).append(' ').append(((Float)dao[DA_SIZE]).floatValue()).append(" Tf ");
                                cb.setColorFill((Color)value);
                                PdfString s = new PdfString(cb.toString());
                                item.getMerged(k).put(PdfName.DA, s);
                                item.getWidget(k).put(PdfName.DA, s);
                                markUsed(item.getWidget(k));
                            }
                        }
                    }
                }
            }
            else if (name.equalsIgnoreCase("textsize")) {
                for (int k = 0; k < item.size(); ++k) {
                    if (hit.isHit(k)) {
                        merged = item.getMerged( k );
                        da = merged.getAsString(PdfName.DA);
                        if (da != null) {
                            Object dao[] = splitDAelements(da.toUnicodeString());
                            PdfAppearance cb = new PdfAppearance();
                            if (dao[DA_FONT] != null) {
                                ByteBuffer buf = cb.getInternalBuffer();
                                buf.append(new PdfName((String)dao[DA_FONT]).getBytes()).append(' ').append(((Float)value).floatValue()).append(" Tf ");
                                if (dao[DA_COLOR] != null)
                                    cb.setColorFill((Color)dao[DA_COLOR]);
                                PdfString s = new PdfString(cb.toString());
                                item.getMerged(k).put(PdfName.DA, s);
                                item.getWidget(k).put(PdfName.DA, s);
                                markUsed(item.getWidget(k));
                            }
                        }
                    }
                }
            }
            else if (name.equalsIgnoreCase("bgcolor") || name.equalsIgnoreCase("bordercolor")) {
                PdfName dname = (name.equalsIgnoreCase("bgcolor") ? PdfName.BG : PdfName.BC);
                for (int k = 0; k < item.size(); ++k) {
                    if (hit.isHit(k)) {
                        merged = item.getMerged( k );
                        PdfDictionary mk = merged.getAsDict(PdfName.MK);
                        if (mk == null) {
                            if (value == null)
                                return true;
                            mk = new PdfDictionary();
                            item.getMerged(k).put(PdfName.MK, mk);
                            item.getWidget(k).put(PdfName.MK, mk);
                            markUsed(item.getWidget(k));
                        } else {
                            markUsed( mk );
                        }
                        if (value == null)
                            mk.remove(dname);
                        else
                            mk.put(dname, PdfFormField.getMKColor((Color)value));
                    }
                }
            }
            else
                return false;
            return true;
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    
    public boolean setFieldProperty(String field, String name, int value, int inst[]) {
        if (writer == null)
            throw new RuntimeException("This AcroFields instance is read-only.");
        Item item = fields.get(field);
        if (item == null)
            return false;
        InstHit hit = new InstHit(inst);
        if (name.equalsIgnoreCase("flags")) {
            PdfNumber num = new PdfNumber(value);
            for (int k = 0; k < item.size(); ++k) {
                if (hit.isHit(k)) {
                    item.getMerged(k).put(PdfName.F, num);
                    item.getWidget(k).put(PdfName.F, num);
                    markUsed(item.getWidget(k));
                }
            }
        }
        else if (name.equalsIgnoreCase("setflags")) {
            for (int k = 0; k < item.size(); ++k) {
                if (hit.isHit(k)) {
                    PdfNumber num = item.getWidget(k).getAsNumber(PdfName.F);
                    int val = 0;
                    if (num != null)
                        val = num.intValue();
                    num = new PdfNumber(val | value);
                    item.getMerged(k).put(PdfName.F, num);
                    item.getWidget(k).put(PdfName.F, num);
                    markUsed(item.getWidget(k));
                }
            }
        }
        else if (name.equalsIgnoreCase("clrflags")) {
            for (int k = 0; k < item.size(); ++k) {
                if (hit.isHit(k)) {
                    PdfDictionary widget = item.getWidget( k );
                    PdfNumber num = widget.getAsNumber(PdfName.F);
                    int val = 0;
                    if (num != null)
                        val = num.intValue();
                    num = new PdfNumber(val & (~value));
                    item.getMerged(k).put(PdfName.F, num);
                    widget.put(PdfName.F, num);
                    markUsed(widget);
                }
            }
        }
        else if (name.equalsIgnoreCase("fflags")) {
            PdfNumber num = new PdfNumber(value);
            for (int k = 0; k < item.size(); ++k) {
                if (hit.isHit(k)) {
                    item.getMerged(k).put(PdfName.FF, num);
                    item.getValue(k).put(PdfName.FF, num);
                    markUsed(item.getValue(k));
                }
            }
        }
        else if (name.equalsIgnoreCase("setfflags")) {
            for (int k = 0; k < item.size(); ++k) {
                if (hit.isHit(k)) {
                    PdfDictionary valDict = item.getValue( k );
                    PdfNumber num = valDict.getAsNumber( PdfName.FF );
                    int val = 0;
                    if (num != null)
                        val = num.intValue();
                    num = new PdfNumber(val | value);
                    item.getMerged(k).put(PdfName.FF, num);
                    valDict.put(PdfName.FF, num);
                    markUsed(valDict);
                }
            }
        }
        else if (name.equalsIgnoreCase("clrfflags")) {
            for (int k = 0; k < item.size(); ++k) {
                if (hit.isHit(k)) {
                    PdfDictionary valDict = item.getValue( k );
                    PdfNumber num = valDict.getAsNumber(PdfName.FF);
                    int val = 0;
                    if (num != null)
                        val = num.intValue();
                    num = new PdfNumber(val & (~value));
                    item.getMerged(k).put(PdfName.FF, num);
                    valDict.put(PdfName.FF, num);
                    markUsed(valDict);
                }
            }
        }
        else
            return false;
        return true;
    }

    
    public void mergeXfaData(Node n) throws IOException, DocumentException {
        XfaForm.Xml2SomDatasets data = new XfaForm.Xml2SomDatasets(n);
        for (String name: data.getOrder()) {
            String text = XfaForm.getNodeText(data.getName2Node().get(name));
            setField(name, text);
        }
    }

    
    public void setFields(FdfReader fdf) throws IOException, DocumentException {
        HashMap<String, PdfDictionary> fd = fdf.getFields();
        for (String f: fd.keySet()) {
            String v = fdf.getFieldValue(f);
            if (v != null)
                setField(f, v);
        }
    }

    
    public void setFields(XfdfReader xfdf) throws IOException, DocumentException {
        HashMap<String, String> fd = xfdf.getFields();
        for (String f: fd.keySet()) {
            String v = xfdf.getFieldValue(f);
            if (v != null)
                setField(f, v);
            List<String> l = xfdf.getListValues(f);
            if (l != null)
                setListSelection(v, l.toArray(new String[l.size()]));
        }
    }

    
    public boolean regenerateField(String name) throws IOException, DocumentException {
        String value = getField(name);
        return setField(name, value, value);
    }

    
    public boolean setField(String name, String value) throws IOException, DocumentException {
        return setField(name, value, null);
    }

    
    public boolean setField(String name, String value, String display) throws IOException, DocumentException {
        if (writer == null)
            throw new DocumentException("This AcroFields instance is read-only.");
        if (xfa.isXfaPresent()) {
            name = xfa.findFieldName(name, this);
            if (name == null)
                return false;
            String shortName = XfaForm.Xml2Som.getShortName(name);
            Node xn = xfa.findDatasetsNode(shortName);
            if (xn == null) {
                xn = xfa.getDatasetsSom().insertNode(xfa.getDatasetsNode(), shortName);
            }
            xfa.setNodeText(xn, value);
        }
        Item item = fields.get(name);
        if (item == null)
            return false;
        PdfDictionary merged = item.getMerged( 0 );
        PdfName type = merged.getAsName(PdfName.FT);
        if (PdfName.TX.equals(type)) {
            PdfNumber maxLen = merged.getAsNumber(PdfName.MAXLEN);
            int len = 0;
            if (maxLen != null)
                len = maxLen.intValue();
            if (len > 0)
                value = value.substring(0, Math.min(len, value.length()));
        }
        if (display == null)
            display = value;
        if (PdfName.TX.equals(type) || PdfName.CH.equals(type)) {
            PdfString v = new PdfString(value, PdfObject.TEXT_UNICODE);
            for (int idx = 0; idx < item.size(); ++idx) {
                PdfDictionary valueDic = item.getValue(idx);
                valueDic.put(PdfName.V, v);
                valueDic.remove(PdfName.I);
                markUsed(valueDic);
                merged = item.getMerged(idx);
                merged.remove(PdfName.I);
                merged.put(PdfName.V, v);
                PdfDictionary widget = item.getWidget(idx);
                if (generateAppearances) {
                    PdfAppearance app = getAppearance(merged, display, name);
                    if (PdfName.CH.equals(type)) {
                        PdfNumber n = new PdfNumber(topFirst);
                        widget.put(PdfName.TI, n);
                        merged.put(PdfName.TI, n);
                    }
                    PdfDictionary appDic = widget.getAsDict(PdfName.AP);
                    if (appDic == null) {
                        appDic = new PdfDictionary();
                        widget.put(PdfName.AP, appDic);
                        merged.put(PdfName.AP, appDic);
                    }
                    appDic.put(PdfName.N, app.getIndirectReference());
                    writer.releaseTemplate(app);
                }
                else {
                    widget.remove(PdfName.AP);
                    merged.remove(PdfName.AP);
                }
                markUsed(widget);
            }
            return true;
        }
        else if (PdfName.BTN.equals(type)) {
            PdfNumber ff = item.getMerged(0).getAsNumber(PdfName.FF);
            int flags = 0;
            if (ff != null)
                flags = ff.intValue();
            if ((flags & PdfFormField.FF_PUSHBUTTON) != 0) {
                
                Image img;
                try {
                    img = Image.getInstance(Base64.decode(value));
                }
                catch (Exception e) {
                    return false;
                }
                PushbuttonField pb = getNewPushbuttonFromField(name);
                pb.setImage(img);
                replacePushbuttonField(name, pb.getField());
                return true;
            }
            PdfName v = new PdfName(value);
            ArrayList<String> lopt = new ArrayList<String>();
            PdfArray opts = item.getValue(0).getAsArray(PdfName.OPT);
            if (opts != null) {
                for (int k = 0; k < opts.size(); ++k) {
                    PdfString valStr = opts.getAsString(k);
                    if (valStr != null)
                        lopt.add(valStr.toUnicodeString());
                    else
                        lopt.add(null);
                }
            }
            int vidx = lopt.indexOf(value);
            PdfName valt = null;
            PdfName vt;
            if (vidx >= 0) {
                vt = valt = new PdfName(String.valueOf(vidx));
            }
            else
                vt = v;
            for (int idx = 0; idx < item.size(); ++idx) {
                merged = item.getMerged(idx);
                PdfDictionary widget = item.getWidget(idx);
                PdfDictionary valDict = item.getValue(idx);
                markUsed(item.getValue(idx));
                if (valt != null) {
                    PdfString ps = new PdfString(value, PdfObject.TEXT_UNICODE);
                    valDict.put(PdfName.V, ps);
                    merged.put(PdfName.V, ps);
                }
                else {
                    valDict.put(PdfName.V, v);
                    merged.put(PdfName.V, v);
                }
                markUsed(widget);
                if (isInAP(widget,  vt)) {
                    merged.put(PdfName.AS, vt);
                    widget.put(PdfName.AS, vt);
                }
                else {
                    merged.put(PdfName.AS, PdfName.Off);
                    widget.put(PdfName.AS, PdfName.Off);
                }
            }
            return true;
        }
        return false;
    }

    
    public boolean setListSelection(String name, String[] value) throws IOException, DocumentException {
        Item item = getFieldItem(name);
        if (item == null)
            return false;
        PdfName type = item.getMerged(0).getAsName(PdfName.FT);
        if (!PdfName.CH.equals(type)) {
            return false;
        }
        String[] options = getListOptionExport(name);
        PdfArray array = new PdfArray();
        for (int i = 0; i < value.length; i++) {
            for (int j = 0; j < options.length; j++) {
                if (options[j].equals(value[i])) {
                    array.add(new PdfNumber(j));
                }
            }
        }
        item.writeToAll(PdfName.I, array, Item.WRITE_MERGED | Item.WRITE_VALUE);
        item.writeToAll(PdfName.V, null, Item.WRITE_MERGED | Item.WRITE_VALUE);
        item.writeToAll(PdfName.AP, null, Item.WRITE_MERGED | Item.WRITE_WIDGET);
        item.markUsed( this, Item.WRITE_VALUE | Item.WRITE_WIDGET );
        return true;
    }

    boolean isInAP(PdfDictionary dic, PdfName check) {
        PdfDictionary appDic = dic.getAsDict(PdfName.AP);
        if (appDic == null)
            return false;
        PdfDictionary NDic = appDic.getAsDict(PdfName.N);
        return (NDic != null && NDic.get(check) != null);
    }

    
    public HashMap<String, Item> getFields() {
        return fields;
    }

    
    public Item getFieldItem(String name) {
        if (xfa.isXfaPresent()) {
            name = xfa.findFieldName(name, this);
            if (name == null)
                return null;
        }
        return fields.get(name);
    }

    
    public String getTranslatedFieldName(String name) {
        if (xfa.isXfaPresent()) {
            String namex = xfa.findFieldName(name, this);
            if (namex != null)
                name = namex;
        }
        return name;
    }

    
    public float[] getFieldPositions(String name) {
        Item item = getFieldItem(name);
        if (item == null)
            return null;
        float ret[] = new float[item.size() * 5];
        int ptr = 0;
        for (int k = 0; k < item.size(); ++k) {
            try {
                PdfDictionary wd = item.getWidget(k);
                PdfArray rect = wd.getAsArray(PdfName.RECT);
                if (rect == null)
                    continue;
                Rectangle r = PdfReader.getNormalizedRectangle(rect);
                int page = item.getPage(k).intValue();
                int rotation = reader.getPageRotation(page);
                ret[ptr++] = page;
                if (rotation != 0) {
                    Rectangle pageSize = reader.getPageSize(page);
                    switch (rotation) {
                        case 270:
                            r = new Rectangle(
                                pageSize.getTop() - r.getBottom(),
                                r.getLeft(),
                                pageSize.getTop() - r.getTop(),
                                r.getRight());
                            break;
                        case 180:
                            r = new Rectangle(
                                pageSize.getRight() - r.getLeft(),
                                pageSize.getTop() - r.getBottom(),
                                pageSize.getRight() - r.getRight(),
                                pageSize.getTop() - r.getTop());
                            break;
                        case 90:
                            r = new Rectangle(
                                r.getBottom(),
                                pageSize.getRight() - r.getLeft(),
                                r.getTop(),
                                pageSize.getRight() - r.getRight());
                            break;
                    }
                    r.normalize();
                }
                ret[ptr++] = r.getLeft();
                ret[ptr++] = r.getBottom();
                ret[ptr++] = r.getRight();
                ret[ptr++] = r.getTop();
            }
            catch (Exception e) {
                
            }
        }
        if (ptr < ret.length) {
            float ret2[] = new float[ptr];
            System.arraycopy(ret, 0, ret2, 0, ptr);
            return ret2;
        }
        return ret;
    }

    private int removeRefFromArray(PdfArray array, PdfObject refo) {
        if (refo == null || !refo.isIndirect())
            return array.size();
        PdfIndirectReference ref = (PdfIndirectReference)refo;
        for (int j = 0; j < array.size(); ++j) {
            PdfObject obj = array.getPdfObject(j);
            if (!obj.isIndirect())
                continue;
            if (((PdfIndirectReference)obj).getNumber() == ref.getNumber())
                array.remove(j--);
        }
        return array.size();
    }

    
    public boolean removeFieldsFromPage(int page) {
        if (page < 1)
            return false;
        String names[] = new String[fields.size()];
        fields.keySet().toArray(names);
        boolean found = false;
        for (int k = 0; k < names.length; ++k) {
            boolean fr = removeField(names[k], page);
            found = (found || fr);
        }
        return found;
    }

    
    public boolean removeField(String name, int page) {
        Item item = getFieldItem(name);
        if (item == null)
            return false;
        PdfDictionary acroForm = (PdfDictionary)PdfReader.getPdfObject(reader.getCatalog().get(PdfName.ACROFORM), reader.getCatalog());

        if (acroForm == null)
            return false;
        PdfArray arrayf = acroForm.getAsArray(PdfName.FIELDS);
        if (arrayf == null)
            return false;
        for (int k = 0; k < item.size(); ++k) {
            int pageV = item.getPage(k).intValue();
            if (page != -1 && page != pageV)
                continue;
            PdfIndirectReference ref = item.getWidgetRef(k);
            PdfDictionary wd = item.getWidget( k );
            PdfDictionary pageDic = reader.getPageN(pageV);
            PdfArray annots = pageDic.getAsArray(PdfName.ANNOTS);
            if (annots != null) {
                if (removeRefFromArray(annots, ref) == 0) {
                    pageDic.remove(PdfName.ANNOTS);
                    markUsed(pageDic);
                }
                else
                    markUsed(annots);
            }
            PdfReader.killIndirect(ref);
            PdfIndirectReference kid = ref;
            while ((ref = wd.getAsIndirectObject(PdfName.PARENT)) != null) {
                wd = wd.getAsDict( PdfName.PARENT );
                PdfArray kids = wd.getAsArray(PdfName.KIDS);
                if (removeRefFromArray(kids, kid) != 0)
                    break;
                kid = ref;
                PdfReader.killIndirect(ref);
            }
            if (ref == null) {
                removeRefFromArray(arrayf, kid);
                markUsed(arrayf);
            }
            if (page != -1) {
                item.remove( k );
                --k;
            }
        }
        if (page == -1 || item.size() == 0)
            fields.remove(name);
        return true;
    }

    
    public boolean removeField(String name) {
        return removeField(name, -1);
    }

    
    public boolean isGenerateAppearances() {
        return generateAppearances;
    }

    
    public void setGenerateAppearances(boolean generateAppearances) {
        this.generateAppearances = generateAppearances;
        PdfDictionary top = reader.getCatalog().getAsDict(PdfName.ACROFORM);
        if (generateAppearances)
            top.remove(PdfName.NEEDAPPEARANCES);
        else
            top.put(PdfName.NEEDAPPEARANCES, PdfBoolean.PDFTRUE);
    }

    
    public static class Item {
        
        
        public static final int WRITE_MERGED = 1;
        
        
        public static final int WRITE_WIDGET = 2;
        
        
        public static final int WRITE_VALUE = 4;

        
        public void writeToAll(PdfName key, PdfObject value, int writeFlags) {
            int i;
            PdfDictionary curDict = null;
            if ((writeFlags & WRITE_MERGED) != 0) {
                for (i = 0; i < merged.size(); ++i) {
                    curDict = getMerged(i);
                    curDict.put(key, value);
                }
            }
            if ((writeFlags & WRITE_WIDGET) != 0) {
                for (i = 0; i < widgets.size(); ++i) {
                    curDict = getWidget(i);
                    curDict.put(key, value);
                }
            }
            if ((writeFlags & WRITE_VALUE) != 0) {
                for (i = 0; i < values.size(); ++i) {
                    curDict = getValue(i);
                    curDict.put(key, value);
                }
            }
        }

        
        public void markUsed( AcroFields parentFields, int writeFlags ) {
            if ((writeFlags & WRITE_VALUE) != 0) {
                for (int i = 0; i < size(); ++i) {
                    parentFields.markUsed( getValue( i ) );
                }
            }
            if ((writeFlags & WRITE_WIDGET) != 0) {
                for (int i = 0; i < size(); ++i) {
                    parentFields.markUsed(getWidget(i));
                }
            }
        }

        
        public ArrayList<PdfDictionary> values = new ArrayList<PdfDictionary>();
        
        
        public ArrayList<PdfDictionary> widgets = new ArrayList<PdfDictionary>();
        
        

        public ArrayList<PdfObject> widget_refs = new ArrayList<PdfObject>();
        
        
        public ArrayList<PdfDictionary> merged = new ArrayList<PdfDictionary>();
        
        
        public ArrayList<Integer> page = new ArrayList<Integer>();
        
        public ArrayList<Integer> tabOrder = new ArrayList<Integer>();

        
        public int size() {
            return values.size();
        }

        
        void remove(int killIdx) {
            values.remove(killIdx);
            widgets.remove(killIdx);
            widget_refs.remove(killIdx);
            merged.remove(killIdx);
            page.remove(killIdx);
            tabOrder.remove(killIdx);
        }

        
        public PdfDictionary getValue(int idx) {
            return values.get(idx);
        }

        
        void addValue(PdfDictionary value) {
            values.add(value);
        }

        
        public PdfDictionary getWidget(int idx) {
            return widgets.get(idx);
        }

        
        void addWidget(PdfDictionary widget) {
            widgets.add(widget);
        }

        
        public PdfIndirectReference getWidgetRef(int idx) {
            return (PdfIndirectReference) widget_refs.get(idx);
        }

        
        void addWidgetRef(PdfIndirectReference widgRef) {
            widget_refs.add(widgRef);
        }

        
        public PdfDictionary getMerged(int idx) {
            return merged.get(idx);
        }

        
        void addMerged(PdfDictionary mergeDict) {
            merged.add(mergeDict);
        }

        
        public Integer getPage(int idx) {
            return page.get(idx);
        }

        
        void addPage(int pg) {
            page.add(new Integer(pg));
        }

        
        void forcePage(int idx, int pg) {
            page.set(idx, new Integer( pg ));
        }

        
        public Integer getTabOrder(int idx) {
            return tabOrder.get(idx);
        }

        
        void addTabOrder(int order) {
            tabOrder.add(new Integer(order));
        }
    }

    private static class InstHit {
        IntHashtable hits;
        public InstHit(int inst[]) {
            if (inst == null)
                return;
            hits = new IntHashtable();
            for (int k = 0; k < inst.length; ++k)
                hits.put(inst[k], 1);
        }

        public boolean isHit(int n) {
            if (hits == null)
                return true;
            return hits.containsKey(n);
        }
    }

    
    public ArrayList<String> getSignatureNames() {
        if (sigNames != null)
            return new ArrayList<String>(sigNames.keySet());
        sigNames = new HashMap<String, int[]>();
        ArrayList<Object[]> sorter = new ArrayList<Object[]>();
        for (Map.Entry<String, Item> entry: fields.entrySet()) {
            Item item = entry.getValue();
            PdfDictionary merged = item.getMerged(0);
            if (!PdfName.SIG.equals(merged.get(PdfName.FT)))
                continue;
            PdfDictionary v = merged.getAsDict(PdfName.V);
            if (v == null)
                continue;
            PdfString contents = v.getAsString(PdfName.CONTENTS);
            if (contents == null)
                continue;
            PdfArray ro = v.getAsArray(PdfName.BYTERANGE);
            if (ro == null)
                continue;
            int rangeSize = ro.size();
            if (rangeSize < 2)
                continue;
            int length = ro.getAsNumber(rangeSize - 1).intValue() + ro.getAsNumber(rangeSize - 2).intValue();
            sorter.add(new Object[]{entry.getKey(), new int[]{length, 0}});
        }
        Collections.sort(sorter, new AcroFields.SorterComparator());
        if (!sorter.isEmpty()) {
            if (((int[])(sorter.get(sorter.size() - 1))[1])[0] == reader.getFileLength())
                totalRevisions = sorter.size();
            else
                totalRevisions = sorter.size() + 1;
            for (int k = 0; k < sorter.size(); ++k) {
                Object objs[] = sorter.get(k);
                String name = (String)objs[0];
                int p[] = (int[])objs[1];
                p[1] = k + 1;
                sigNames.put(name, p);
            }
        }
        return new ArrayList<String>(sigNames.keySet());
    }

    
    public ArrayList<String> getBlankSignatureNames() {
        getSignatureNames();
        ArrayList<String> sigs = new ArrayList<String>();
        for (Map.Entry<String, Item> entry: fields.entrySet()) {
            Item item = entry.getValue();
            PdfDictionary merged = item.getMerged(0);
            if (!PdfName.SIG.equals(merged.getAsName(PdfName.FT)))
                continue;
            if (sigNames.containsKey(entry.getKey()))
                continue;
            sigs.add(entry.getKey());
        }
        return sigs;
    }

    
    public PdfDictionary getSignatureDictionary(String name) {
        getSignatureNames();
        name = getTranslatedFieldName(name);
        if (!sigNames.containsKey(name))
            return null;
        Item item = fields.get(name);
        PdfDictionary merged = item.getMerged(0);
        return merged.getAsDict(PdfName.V);
    }

    
    public boolean signatureCoversWholeDocument(String name) {
        getSignatureNames();
        name = getTranslatedFieldName(name);
        if (!sigNames.containsKey(name))
            return false;
        return sigNames.get(name)[0] == reader.getFileLength();
    }

    
    public PdfPKCS7 verifySignature(String name) {
        return verifySignature(name, null);
    }

    
    public PdfPKCS7 verifySignature(String name, String provider) {
        PdfDictionary v = getSignatureDictionary(name);
        if (v == null)
            return null;
        try {
            PdfName sub = v.getAsName(PdfName.SUBFILTER);
            PdfString contents = v.getAsString(PdfName.CONTENTS);
            PdfPKCS7 pk = null;
            if (sub.equals(PdfName.ADBE_X509_RSA_SHA1)) {
                PdfString cert = v.getAsString(PdfName.CERT);
                pk = new PdfPKCS7(contents.getOriginalBytes(), cert.getBytes(), provider);
            }
            else
                pk = new PdfPKCS7(contents.getOriginalBytes(), provider);
            updateByteRange(pk, v);
            PdfString str = v.getAsString(PdfName.M);
            if (str != null)
                pk.setSignDate(PdfDate.decode(str.toString()));
            PdfObject obj = PdfReader.getPdfObject(v.get(PdfName.NAME));
            if (obj != null) {
              if (obj.isString())
                pk.setSignName(((PdfString)obj).toUnicodeString());
              else if(obj.isName())
                pk.setSignName(PdfName.decodeName(obj.toString()));
            }
            str = v.getAsString(PdfName.REASON);
            if (str != null)
                pk.setReason(str.toUnicodeString());
            str = v.getAsString(PdfName.LOCATION);
            if (str != null)
                pk.setLocation(str.toUnicodeString());
            return pk;
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    private void updateByteRange(PdfPKCS7 pkcs7, PdfDictionary v) {
        PdfArray b = v.getAsArray(PdfName.BYTERANGE);
        RandomAccessFileOrArray rf = reader.getSafeFile();
        try {
            rf.reOpen();
            byte buf[] = new byte[8192];
            for (int k = 0; k < b.size(); ++k) {
                int start = b.getAsNumber(k).intValue();
                int length = b.getAsNumber(++k).intValue();
                rf.seek(start);
                while (length > 0) {
                    int rd = rf.read(buf, 0, Math.min(length, buf.length));
                    if (rd <= 0)
                        break;
                    length -= rd;
                    pkcs7.update(buf, 0, rd);
                }
            }
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
        finally {
            try{rf.close();}catch(Exception e){}
        }
    }

    private void markUsed(PdfObject obj) {
        if (!append)
            return;
        ((PdfStamperImp)writer).markUsed(obj);
    }

    
    public int getTotalRevisions() {
        getSignatureNames();
        return this.totalRevisions;
    }

    
    public int getRevision(String field) {
        getSignatureNames();
        field = getTranslatedFieldName(field);
        if (!sigNames.containsKey(field))
            return 0;
        return sigNames.get(field)[1];
    }

    
    public InputStream extractRevision(String field) throws IOException {
        getSignatureNames();
        field = getTranslatedFieldName(field);
        if (!sigNames.containsKey(field))
            return null;
        int length = sigNames.get(field)[0];
        RandomAccessFileOrArray raf = reader.getSafeFile();
        raf.reOpen();
        raf.seek(0);
        return new RevisionStream(raf, length);
    }

    
    public HashMap<String, TextField> getFieldCache() {
        return this.fieldCache;
    }

    
    public void setFieldCache(HashMap<String, TextField> fieldCache) {
        this.fieldCache = fieldCache;
    }

    
    public void setExtraMargin(float extraMarginLeft, float extraMarginTop) {
        this.extraMarginLeft = extraMarginLeft;
        this.extraMarginTop = extraMarginTop;
    }

    
    public void addSubstitutionFont(BaseFont font) {
        if (substitutionFonts == null)
            substitutionFonts = new ArrayList<BaseFont>();
        substitutionFonts.add(font);
    }

    private static final HashMap<String, String[]> stdFieldFontNames = new HashMap<String, String[]>();

    
    private int totalRevisions;

    
    private HashMap<String, TextField> fieldCache;
    
    static {
        stdFieldFontNames.put("CoBO", new String[]{"Courier-BoldOblique"});
        stdFieldFontNames.put("CoBo", new String[]{"Courier-Bold"});
        stdFieldFontNames.put("CoOb", new String[]{"Courier-Oblique"});
        stdFieldFontNames.put("Cour", new String[]{"Courier"});
        stdFieldFontNames.put("HeBO", new String[]{"Helvetica-BoldOblique"});
        stdFieldFontNames.put("HeBo", new String[]{"Helvetica-Bold"});
        stdFieldFontNames.put("HeOb", new String[]{"Helvetica-Oblique"});
        stdFieldFontNames.put("Helv", new String[]{"Helvetica"});
        stdFieldFontNames.put("Symb", new String[]{"Symbol"});
        stdFieldFontNames.put("TiBI", new String[]{"Times-BoldItalic"});
        stdFieldFontNames.put("TiBo", new String[]{"Times-Bold"});
        stdFieldFontNames.put("TiIt", new String[]{"Times-Italic"});
        stdFieldFontNames.put("TiRo", new String[]{"Times-Roman"});
        stdFieldFontNames.put("ZaDb", new String[]{"ZapfDingbats"});
        stdFieldFontNames.put("HySm", new String[]{"HYSMyeongJo-Medium", "UniKS-UCS2-H"});
        stdFieldFontNames.put("HyGo", new String[]{"HYGoThic-Medium", "UniKS-UCS2-H"});
        stdFieldFontNames.put("KaGo", new String[]{"HeiseiKakuGo-W5", "UniKS-UCS2-H"});
        stdFieldFontNames.put("KaMi", new String[]{"HeiseiMin-W3", "UniJIS-UCS2-H"});
        stdFieldFontNames.put("MHei", new String[]{"MHei-Medium", "UniCNS-UCS2-H"});
        stdFieldFontNames.put("MSun", new String[]{"MSung-Light", "UniCNS-UCS2-H"});
        stdFieldFontNames.put("STSo", new String[]{"STSong-Light", "UniGB-UCS2-H"});
    }

    private static class RevisionStream extends InputStream {
        private byte b[] = new byte[1];
        private RandomAccessFileOrArray raf;
        private int length;
        private int rangePosition = 0;
        private boolean closed;

        private RevisionStream(RandomAccessFileOrArray raf, int length) {
            this.raf = raf;
            this.length = length;
        }

        public int read() throws IOException {
            int n = read(b);
            if (n != 1)
                return -1;
            return b[0] & 0xff;
        }

        public int read(byte[] b, int off, int len) throws IOException {
            if (b == null) {
                throw new NullPointerException();
            } else if ((off < 0) || (off > b.length) || (len < 0) ||
            ((off + len) > b.length) || ((off + len) < 0)) {
                throw new IndexOutOfBoundsException();
            } else if (len == 0) {
                return 0;
            }
            if (rangePosition >= length) {
                close();
                return -1;
            }
            int elen = Math.min(len, length - rangePosition);
            raf.readFully(b, off, elen);
            rangePosition += elen;
            return elen;
        }

        public void close() throws IOException {
            if (!closed) {
                raf.close();
                closed = true;
            }
        }
    }
    
    private static class SorterComparator implements Comparator<Object[]> {        
        public int compare(Object[] o1, Object[] o2) {
            int n1 = ((int[])o1[1])[0];
            int n2 = ((int[])o2[1])[0];
            return n1 - n2;
        }
    }

    
    public ArrayList<BaseFont> getSubstitutionFonts() {
        return substitutionFonts;
    }

    
    public void setSubstitutionFonts(ArrayList<BaseFont> substitutionFonts) {
        this.substitutionFonts = substitutionFonts;
    }

    
    public XfaForm getXfa() {
        return xfa;
    }

    private static final PdfName[] buttonRemove = {PdfName.MK, PdfName.F , PdfName.FF , PdfName.Q , PdfName.BS , PdfName.BORDER};

    
    public PushbuttonField getNewPushbuttonFromField(String field) {
        return getNewPushbuttonFromField(field, 0);
    }

    
    public PushbuttonField getNewPushbuttonFromField(String field, int order) {
        try {
            if (getFieldType(field) != FIELD_TYPE_PUSHBUTTON)
                return null;
            Item item = getFieldItem(field);
            if (order >= item.size())
                return null;
            int posi = order * 5;
            float[] pos = getFieldPositions(field);
            Rectangle box = new Rectangle(pos[posi + 1], pos[posi + 2], pos[posi + 3], pos[posi + 4]);
            PushbuttonField newButton = new PushbuttonField(writer, box, null);
            PdfDictionary dic = item.getMerged(order);
            decodeGenericDictionary(dic, newButton);
            PdfDictionary mk = dic.getAsDict(PdfName.MK);
            if (mk != null) {
                PdfString text = mk.getAsString(PdfName.CA);
                if (text != null)
                    newButton.setText(text.toUnicodeString());
                PdfNumber tp = mk.getAsNumber(PdfName.TP);
                if (tp != null)
                    newButton.setLayout(tp.intValue() + 1);
                PdfDictionary ifit = mk.getAsDict(PdfName.IF);
                if (ifit != null) {
                    PdfName sw = ifit.getAsName(PdfName.SW);
                    if (sw != null) {
                        int scale = PushbuttonField.SCALE_ICON_ALWAYS;
                        if (sw.equals(PdfName.B))
                            scale = PushbuttonField.SCALE_ICON_IS_TOO_BIG;
                        else if (sw.equals(PdfName.S))
                            scale = PushbuttonField.SCALE_ICON_IS_TOO_SMALL;
                        else if (sw.equals(PdfName.N))
                            scale = PushbuttonField.SCALE_ICON_NEVER;
                        newButton.setScaleIcon(scale);
                    }
                    sw = ifit.getAsName(PdfName.S);
                    if (sw != null) {
                        if (sw.equals(PdfName.A))
                            newButton.setProportionalIcon(false);
                    }
                    PdfArray aj = ifit.getAsArray(PdfName.A);
                    if (aj != null && aj.size() == 2) {
                        float left = aj.getAsNumber(0).floatValue();
                        float bottom = aj.getAsNumber(1).floatValue();
                        newButton.setIconHorizontalAdjustment(left);
                        newButton.setIconVerticalAdjustment(bottom);
                    }
                    PdfBoolean fb = ifit.getAsBoolean(PdfName.FB);
                    if (fb != null && fb.booleanValue())
                        newButton.setIconFitToBounds(true);
                }
                PdfObject i = mk.get(PdfName.I);
                if (i != null && i.isIndirect())
                    newButton.setIconReference((PRIndirectReference)i);
            }
            return newButton;
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    
    public boolean replacePushbuttonField(String field, PdfFormField button) {
        return replacePushbuttonField(field, button, 0);
    }

    
    public boolean replacePushbuttonField(String field, PdfFormField button, int order) {
        if (getFieldType(field) != FIELD_TYPE_PUSHBUTTON)
            return false;
        Item item = getFieldItem(field);
        if (order >= item.size())
            return false;
        PdfDictionary merged = item.getMerged(order);
        PdfDictionary values = item.getValue(order);
        PdfDictionary widgets = item.getWidget(order);
        for (int k = 0; k < buttonRemove.length; ++k) {
            merged.remove(buttonRemove[k]);
            values.remove(buttonRemove[k]);
            widgets.remove(buttonRemove[k]);
        }
        for (PdfName key: button.getKeys()) {
            if (key.equals(PdfName.T) || key.equals(PdfName.RECT))
                continue;
            if (key.equals(PdfName.FF))
                values.put(key, button.get(key));
            else
                widgets.put(key, button.get(key));
            merged.put(key, button.get(key));
        }
        return true;
    }

}
