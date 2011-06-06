

package com.lowagie.text.pdf;

import com.lowagie.text.DocumentException;
import java.io.OutputStream;
import java.util.HashMap;


class PdfCopyFormsImp extends PdfCopyFieldsImp {

    
    PdfCopyFormsImp(OutputStream os) throws DocumentException {
        super(os);
    }
    
    
    public void copyDocumentFields(PdfReader reader) throws DocumentException {
        if (!reader.isOpenedWithFullPermissions())
            throw new IllegalArgumentException("PdfReader not opened with owner password");
        if (readers2intrefs.containsKey(reader)) {
            reader = new PdfReader(reader);
        }
        else {
            if (reader.isTampered())
                throw new DocumentException("The document was reused.");
            reader.consolidateNamedDestinations();
            reader.setTampered(true);
        }
        reader.shuffleSubsetNames();
        readers2intrefs.put(reader, new IntHashtable());
        fields.add(reader.getAcroFields());
        updateCalculationOrder(reader);
    }

    
    void mergeFields() {
        for (int k = 0; k < fields.size(); ++k) {
            HashMap fd = ((AcroFields)fields.get(k)).getFields();
            mergeWithMaster(fd);
        }
    }

}