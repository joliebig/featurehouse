

package com.lowagie.text.pdf.internal;

import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfBoolean;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfWriter;
import com.lowagie.text.pdf.interfaces.PdfViewerPreferences;



public class PdfViewerPreferencesImp implements PdfViewerPreferences {
    public static final PdfName[] VIEWER_PREFERENCES = {
            PdfName.HIDETOOLBAR,            
            PdfName.HIDEMENUBAR,            
            PdfName.HIDEWINDOWUI,           
            PdfName.FITWINDOW,              
            PdfName.CENTERWINDOW,            
            PdfName.DISPLAYDOCTITLE,        
            PdfName.NONFULLSCREENPAGEMODE,    
            PdfName.DIRECTION,                
            PdfName.VIEWAREA,                
            PdfName.VIEWCLIP,                
            PdfName.PRINTAREA,                
            PdfName.PRINTCLIP,                
            PdfName.PRINTSCALING,            
            PdfName.DUPLEX,                    
            PdfName.PICKTRAYBYPDFSIZE,        
            PdfName.PRINTPAGERANGE,            
            PdfName.NUMCOPIES                
        };


    
    public static final PdfName NONFULLSCREENPAGEMODE_PREFERENCES[] = {
        PdfName.USENONE, PdfName.USEOUTLINES, PdfName.USETHUMBS, PdfName.USEOC
    };
    
    public static final PdfName DIRECTION_PREFERENCES[] = {
        PdfName.L2R, PdfName.R2L
    };
    
    public static final PdfName PAGE_BOUNDARIES[] = {
        PdfName.MEDIABOX, PdfName.CROPBOX, PdfName.BLEEDBOX, PdfName.TRIMBOX, PdfName.ARTBOX
    };
    
    public static final PdfName PRINTSCALING_PREFERENCES[] = {
        PdfName.APPDEFAULT, PdfName.NONE
    };
    
    public static final PdfName DUPLEX_PREFERENCES[] = {
        PdfName.SIMPLEX, PdfName.DUPLEXFLIPSHORTEDGE, PdfName.DUPLEXFLIPLONGEDGE
    };
    
    
    private int pageLayoutAndMode = 0;
    
    
    private PdfDictionary viewerPreferences = new PdfDictionary();
    
    
    private static final int viewerPreferencesMask = 0xfff000;

    
    public int getPageLayoutAndMode() {
        return pageLayoutAndMode;
    }

    
    public PdfDictionary getViewerPreferences() {
        return viewerPreferences;
    }
    
    
    public void setViewerPreferences(int preferences) {
        this.pageLayoutAndMode |= preferences;
        
        
        if ((preferences & viewerPreferencesMask) != 0) {
            pageLayoutAndMode = ~viewerPreferencesMask & pageLayoutAndMode;
            if ((preferences & PdfWriter.HideToolbar) != 0)
                viewerPreferences.put(PdfName.HIDETOOLBAR, PdfBoolean.PDFTRUE);
            if ((preferences & PdfWriter.HideMenubar) != 0)
                viewerPreferences.put(PdfName.HIDEMENUBAR, PdfBoolean.PDFTRUE);
            if ((preferences & PdfWriter.HideWindowUI) != 0)
                viewerPreferences.put(PdfName.HIDEWINDOWUI, PdfBoolean.PDFTRUE);
            if ((preferences & PdfWriter.FitWindow) != 0)
                viewerPreferences.put(PdfName.FITWINDOW, PdfBoolean.PDFTRUE);
            if ((preferences & PdfWriter.CenterWindow) != 0)
                viewerPreferences.put(PdfName.CENTERWINDOW, PdfBoolean.PDFTRUE);
            if ((preferences & PdfWriter.DisplayDocTitle) != 0)
                viewerPreferences.put(PdfName.DISPLAYDOCTITLE, PdfBoolean.PDFTRUE);
            
            if ((preferences & PdfWriter.NonFullScreenPageModeUseNone) != 0)
                viewerPreferences.put(PdfName.NONFULLSCREENPAGEMODE, PdfName.USENONE);
            else if ((preferences & PdfWriter.NonFullScreenPageModeUseOutlines) != 0)
                viewerPreferences.put(PdfName.NONFULLSCREENPAGEMODE, PdfName.USEOUTLINES);
            else if ((preferences & PdfWriter.NonFullScreenPageModeUseThumbs) != 0)
                viewerPreferences.put(PdfName.NONFULLSCREENPAGEMODE, PdfName.USETHUMBS);
            else if ((preferences & PdfWriter.NonFullScreenPageModeUseOC) != 0)
                viewerPreferences.put(PdfName.NONFULLSCREENPAGEMODE, PdfName.USEOC);

            if ((preferences & PdfWriter.DirectionL2R) != 0)
                viewerPreferences.put(PdfName.DIRECTION, PdfName.L2R);
            else if ((preferences & PdfWriter.DirectionR2L) != 0)
                viewerPreferences.put(PdfName.DIRECTION, PdfName.R2L);

            if ((preferences & PdfWriter.PrintScalingNone) != 0)
                viewerPreferences.put(PdfName.PRINTSCALING, PdfName.NONE);            
        }
    }
    
    
    private int getIndex(PdfName key) {
        for (int i = 0; i < VIEWER_PREFERENCES.length; i++) {
            if (VIEWER_PREFERENCES[i].equals(key))
                return i;
        }
        return -1;
    }
    
    
    private boolean isPossibleValue(PdfName value, PdfName[] accepted) {
        for (int i = 0; i < accepted.length; i++) {
            if (accepted[i].equals(value)) {
                return true;
            }
        }
        return false;
    }
    
    
    public void addViewerPreference(PdfName key, PdfObject value) {
        switch(getIndex(key)) {
        case 0: 
        case 1: 
        case 2: 
        case 3: 
        case 4: 
        case 5: 
        case 14: 
            if (value instanceof PdfBoolean) {
                viewerPreferences.put(key, value);
            }
            break;
        case 6: 
            if (value instanceof PdfName
                    && isPossibleValue((PdfName)value, NONFULLSCREENPAGEMODE_PREFERENCES)) {
                viewerPreferences.put(key, value);
            }
            break;
        case 7: 
            if (value instanceof PdfName
                    && isPossibleValue((PdfName)value, DIRECTION_PREFERENCES)) {
                viewerPreferences.put(key, value);
            }
            break;
        case 8:  
        case 9:  
        case 10: 
        case 11: 
            if (value instanceof PdfName
                    && isPossibleValue((PdfName)value, PAGE_BOUNDARIES)) {
                viewerPreferences.put(key, value);
            }
            break;
        case 12: 
            if (value instanceof PdfName
                    && isPossibleValue((PdfName)value, PRINTSCALING_PREFERENCES)) {
                viewerPreferences.put(key, value);
            }
            break;
        case 13: 
            if (value instanceof PdfName
                    && isPossibleValue((PdfName)value, DUPLEX_PREFERENCES)) {
                viewerPreferences.put(key, value);
            }
            break;
        case 15: 
            if (value instanceof PdfArray) {
                viewerPreferences.put(key, value);
            }
            break;
        case 16: 
            if (value instanceof PdfNumber)  {
                viewerPreferences.put(key, value);
            }
            break;
        }
    }

    
    public void addToCatalog(PdfDictionary catalog) {
        
        catalog.remove(PdfName.PAGELAYOUT);
        if ((pageLayoutAndMode & PdfWriter.PageLayoutSinglePage) != 0)
            catalog.put(PdfName.PAGELAYOUT, PdfName.SINGLEPAGE);
        else if ((pageLayoutAndMode & PdfWriter.PageLayoutOneColumn) != 0)
            catalog.put(PdfName.PAGELAYOUT, PdfName.ONECOLUMN);
        else if ((pageLayoutAndMode & PdfWriter.PageLayoutTwoColumnLeft) != 0)
            catalog.put(PdfName.PAGELAYOUT, PdfName.TWOCOLUMNLEFT);
        else if ((pageLayoutAndMode & PdfWriter.PageLayoutTwoColumnRight) != 0)
            catalog.put(PdfName.PAGELAYOUT, PdfName.TWOCOLUMNRIGHT);
        else if ((pageLayoutAndMode & PdfWriter.PageLayoutTwoPageLeft) != 0)
            catalog.put(PdfName.PAGELAYOUT, PdfName.TWOPAGELEFT);
        else if ((pageLayoutAndMode & PdfWriter.PageLayoutTwoPageRight) != 0)
            catalog.put(PdfName.PAGELAYOUT, PdfName.TWOPAGERIGHT);

        
        catalog.remove(PdfName.PAGEMODE);
        if ((pageLayoutAndMode & PdfWriter.PageModeUseNone) != 0)
            catalog.put(PdfName.PAGEMODE, PdfName.USENONE);
        else if ((pageLayoutAndMode & PdfWriter.PageModeUseOutlines) != 0)
            catalog.put(PdfName.PAGEMODE, PdfName.USEOUTLINES);
        else if ((pageLayoutAndMode & PdfWriter.PageModeUseThumbs) != 0)
            catalog.put(PdfName.PAGEMODE, PdfName.USETHUMBS);
        else if ((pageLayoutAndMode & PdfWriter.PageModeFullScreen) != 0)
            catalog.put(PdfName.PAGEMODE, PdfName.FULLSCREEN);
        else if ((pageLayoutAndMode & PdfWriter.PageModeUseOC) != 0)
            catalog.put(PdfName.PAGEMODE, PdfName.USEOC);
        else if ((pageLayoutAndMode & PdfWriter.PageModeUseAttachments) != 0)
            catalog.put(PdfName.PAGEMODE, PdfName.USEATTACHMENTS);

        
        catalog.remove(PdfName.VIEWERPREFERENCES);
        if (viewerPreferences.size() > 0) {
            catalog.put(PdfName.VIEWERPREFERENCES, viewerPreferences);
        }
    }

    public static PdfViewerPreferencesImp getViewerPreferences(PdfDictionary catalog) {
        PdfViewerPreferencesImp preferences = new PdfViewerPreferencesImp();
        int prefs = 0;
        PdfName name = null;
        
        PdfObject obj = PdfReader.getPdfObjectRelease(catalog.get(PdfName.PAGELAYOUT));
        if (obj != null && obj.isName()) {
            name = (PdfName) obj;
            if (name.equals(PdfName.SINGLEPAGE))
                prefs |= PdfWriter.PageLayoutSinglePage;
            else if (name.equals(PdfName.ONECOLUMN))
                prefs |= PdfWriter.PageLayoutOneColumn;
            else if (name.equals(PdfName.TWOCOLUMNLEFT))
                prefs |= PdfWriter.PageLayoutTwoColumnLeft;
            else if (name.equals(PdfName.TWOCOLUMNRIGHT))
                prefs |= PdfWriter.PageLayoutTwoColumnRight;
            else if (name.equals(PdfName.TWOPAGELEFT))
                prefs |= PdfWriter.PageLayoutTwoPageLeft;
            else if (name.equals(PdfName.TWOPAGERIGHT))
                prefs |= PdfWriter.PageLayoutTwoPageRight;
        }
        
        obj = PdfReader.getPdfObjectRelease(catalog.get(PdfName.PAGEMODE));
        if (obj != null && obj.isName()) {
            name = (PdfName) obj;
            if (name.equals(PdfName.USENONE))
                prefs |= PdfWriter.PageModeUseNone;
            else if (name.equals(PdfName.USEOUTLINES))
                prefs |= PdfWriter.PageModeUseOutlines;
            else if (name.equals(PdfName.USETHUMBS))
                prefs |= PdfWriter.PageModeUseThumbs;
            else if (name.equals(PdfName.FULLSCREEN))
                prefs |= PdfWriter.PageModeFullScreen;
            else if (name.equals(PdfName.USEOC))
                prefs |= PdfWriter.PageModeUseOC;
            else if (name.equals(PdfName.USEATTACHMENTS))
                prefs |= PdfWriter.PageModeUseAttachments;
        }
        
        preferences.setViewerPreferences(prefs);
        
        obj = PdfReader.getPdfObjectRelease(catalog
                .get(PdfName.VIEWERPREFERENCES));
        if (obj != null && obj.isDictionary()) {
            PdfDictionary vp = (PdfDictionary) obj;
            for (int i = 0; i < VIEWER_PREFERENCES.length; i++) {
                obj = PdfReader.getPdfObjectRelease(vp.get(VIEWER_PREFERENCES[i]));
                preferences.addViewerPreference(VIEWER_PREFERENCES[i], obj);
            }
        }
        return preferences;
    }
}