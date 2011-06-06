
package com.lowagie.text.pdf;

import java.util.HashMap;
import java.util.Iterator;

class PageResources {
    
    protected PdfDictionary fontDictionary = new PdfDictionary();
    protected PdfDictionary xObjectDictionary = new PdfDictionary();
    protected PdfDictionary colorDictionary = new PdfDictionary();
    protected PdfDictionary patternDictionary = new PdfDictionary();
    protected PdfDictionary shadingDictionary = new PdfDictionary();
    protected PdfDictionary extGStateDictionary = new PdfDictionary();
    protected PdfDictionary propertyDictionary = new PdfDictionary();
    protected HashMap forbiddenNames;
    protected PdfDictionary originalResources;
    protected int namePtr[] = {0};
    protected HashMap usedNames;

    PageResources() {
    }
    
    void setOriginalResources(PdfDictionary resources, int newNamePtr[]) {
        if (newNamePtr != null)
            namePtr = newNamePtr;
        forbiddenNames = new HashMap();
        usedNames = new HashMap();
        if (resources == null)
            return;
        originalResources = new PdfDictionary();
        originalResources.merge(resources);
        for (Iterator i = resources.getKeys().iterator(); i.hasNext();) {
            PdfName key = (PdfName)i.next();
            PdfObject sub = PdfReader.getPdfObject(resources.get(key));
            if (sub != null && sub.isDictionary()) {
                PdfDictionary dic = (PdfDictionary)sub;
                for (Iterator j = dic.getKeys().iterator(); j.hasNext();) {
                    forbiddenNames.put(j.next(), null);
                }
                PdfDictionary dic2 = new PdfDictionary();
                dic2.merge(dic);
                originalResources.put(key, dic2);
            }
        }
    }
    
    PdfName translateName(PdfName name) {
        PdfName translated = name;
        if (forbiddenNames != null) {
            translated = (PdfName)usedNames.get(name);
            if (translated == null) {
                while (true) {
                    translated = new PdfName("Xi" + (namePtr[0]++));
                    if (!forbiddenNames.containsKey(translated))
                        break;
                }
                usedNames.put(name, translated);
            }
        }
        return translated;
    }
    
    PdfName addFont(PdfName name, PdfIndirectReference reference) {
        name = translateName(name);
        fontDictionary.put(name, reference);
        return name;
    }

    PdfName addXObject(PdfName name, PdfIndirectReference reference) {
        name = translateName(name);
        xObjectDictionary.put(name, reference);
        return name;
    }

    PdfName addColor(PdfName name, PdfIndirectReference reference) {
        name = translateName(name);
        colorDictionary.put(name, reference);
        return name;
    }

    void addDefaultColor(PdfName name, PdfObject obj) {
        if (obj == null || obj.isNull())
            colorDictionary.remove(name);
        else
            colorDictionary.put(name, obj);
    }

    void addDefaultColor(PdfDictionary dic) {
        colorDictionary.merge(dic);
    }

    void addDefaultColorDiff(PdfDictionary dic) {
        colorDictionary.mergeDifferent(dic);
    }

    PdfName addShading(PdfName name, PdfIndirectReference reference) {
        name = translateName(name);
        shadingDictionary.put(name, reference);
        return name;
    }
    
    PdfName addPattern(PdfName name, PdfIndirectReference reference) {
        name = translateName(name);
        patternDictionary.put(name, reference);
        return name;
    }

    PdfName addExtGState(PdfName name, PdfIndirectReference reference) {
        name = translateName(name);
        extGStateDictionary.put(name, reference);
        return name;
    }

    PdfName addProperty(PdfName name, PdfIndirectReference reference) {
        name = translateName(name);
        propertyDictionary.put(name, reference);
        return name;
    }

    PdfDictionary getResources() {
       PdfResources resources = new PdfResources();
        if (originalResources != null)
            resources.putAll(originalResources);
        resources.put(PdfName.PROCSET, new PdfLiteral("[/PDF /Text /ImageB /ImageC /ImageI]"));
        resources.add(PdfName.FONT, fontDictionary);
        resources.add(PdfName.XOBJECT, xObjectDictionary);
        resources.add(PdfName.COLORSPACE, colorDictionary);
        resources.add(PdfName.PATTERN, patternDictionary);
        resources.add(PdfName.SHADING, shadingDictionary);
        resources.add(PdfName.EXTGSTATE, extGStateDictionary);
        resources.add(PdfName.PROPERTIES, propertyDictionary);
        return resources;
    }
    
    boolean hasResources() {
        return (fontDictionary.size() > 0
            || xObjectDictionary.size() > 0
            || colorDictionary.size() > 0
            || patternDictionary.size() > 0
            || shadingDictionary.size() > 0
            || extGStateDictionary.size() > 0
            || propertyDictionary.size() > 0);
    }
}