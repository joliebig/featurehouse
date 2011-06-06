


package com.lowagie.toolbox.swing;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;

import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingConstants;

import com.lowagie.text.pdf.PdfDate;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.RandomAccessFileOrArray;


public class PdfInformationPanel extends JPanel implements PropertyChangeListener {

    
    private static final long serialVersionUID = -4171577284617028707L;

    
    String filename = "";

    
    JLabel label = new JLabel();

    
    JScrollPane scrollpane = new JScrollPane();

    
    JPanel panel = new JPanel();

    
    public PdfInformationPanel() {
        try {
            this.setLayout(new BorderLayout());
            label.setHorizontalAlignment(SwingConstants.CENTER);
            panel.setLayout(new BorderLayout());
            this.add(panel, BorderLayout.CENTER);
            scrollpane.setPreferredSize(new Dimension(200, 200));
            panel.add(scrollpane, BorderLayout.CENTER);
            scrollpane.setViewportView(label);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    
    public void createTextFromPDF(File file) {
        if (file.exists()) {
            int page = 1;
            PdfReader reader = null;

            try {
                reader = new PdfReader(new RandomAccessFileOrArray(file.getAbsolutePath()), null);
                HashMap<String, String> pdfinfo = reader.getInfo();

                StringBuffer sb = new StringBuffer();
                sb.append("<html>=== Document Information ===<p>");
                sb.append(reader.getCropBox(page).getHeight() + "*"
                        + reader.getCropBox(page).getWidth() + "<p>");
                sb.append("PDF Version: " + reader.getPdfVersion() + "<p>");
                sb.append("Number of pages: " + reader.getNumberOfPages()
                        + "<p>");
                sb.append("Number of PDF objects: " + reader.getXrefSize()
                        + "<p>");
                sb.append("File length: " + reader.getFileLength() + "<p>");
                sb.append("Encrypted= " + reader.isEncrypted() + "<p>");
                if (pdfinfo.get("Title") != null) {
                    sb.append("Title= " + pdfinfo.get("Title") + "<p>");
                }
                if (pdfinfo.get("Author") != null) {
                    sb.append("Author= " + pdfinfo.get("Author") + "<p>");
                }
                if (pdfinfo.get("Subject") != null) {
                    sb.append("Subject= " + pdfinfo.get("Subject") + "<p>");
                }
                if (pdfinfo.get("Producer") != null) {
                    sb.append("Producer= " + pdfinfo.get("Producer") + "<p>");
                }
                if (pdfinfo.get("ModDate") != null) {
                    sb.append("ModDate= "
                            + PdfDate.decode(pdfinfo.get("ModDate").toString())
                                    .getTime() + "<p>");
                }
                if (pdfinfo.get("CreationDate") != null) {
                    sb.append("CreationDate= "
                            + PdfDate.decode(
                                    pdfinfo.get("CreationDate").toString())
                                    .getTime() + "<p>");
                }
                sb.append("</html>");
                label.setText(sb.toString());
            } catch (IOException ex) {
                label.setText("");
            }
        }
    }

    
    public void propertyChange(PropertyChangeEvent evt) {
        filename = evt.getPropertyName();
        if (filename.equals(JFileChooser.SELECTED_FILE_CHANGED_PROPERTY)) {
            File file = (File) evt.getNewValue();
            if (file != null) {
                this.createTextFromPDF(file);
                this.repaint();
            }
        }
    }
}
