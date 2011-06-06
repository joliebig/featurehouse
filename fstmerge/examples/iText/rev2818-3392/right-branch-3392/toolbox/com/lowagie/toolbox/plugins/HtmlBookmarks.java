
package com.lowagie.toolbox.plugins;

import java.io.File;
import java.io.FileOutputStream;
import java.util.HashMap;
import java.util.List;
import java.util.StringTokenizer;

import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;

import com.lowagie.text.Anchor;
import com.lowagie.text.Chapter;
import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.Header;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Section;
import com.lowagie.text.html.HtmlTags;
import com.lowagie.text.html.HtmlWriter;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.SimpleBookmark;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.StringArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;
import com.lowagie.tools.Executable;


public class HtmlBookmarks extends AbstractTool {

    static {
        addVersion("$Id: HtmlBookmarks.java 3307 2008-05-01 19:55:48Z xlv $");
    }

    
    public HtmlBookmarks() {
        arguments.add(new FileArgument(this, "srcfile", "The file you want to inspect", false, new PdfFilter()));
        arguments.add(new StringArgument(this, "ownerpassword", "The owner password if the file is encrypt"));
        arguments.add(new StringArgument(this, "css", "The path to a CSS file"));
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("Html Bookmarks", true, true, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== Html Bookmarks OPENED ===");
    }

    
    public void execute() {
        try {
            if (getValue("srcfile") == null) throw new InstantiationException("You need to choose a sourcefile");
            File src = (File)getValue("srcfile");
            PdfReader reader;
            if (getValue("ownerpassword") == null) {
                reader = new PdfReader(src.getAbsolutePath());
            }
            else {
                reader = new PdfReader(src.getAbsolutePath(), ((String)getValue("ownerpassword")).getBytes());
            }
            File directory = src.getParentFile();
            String name = src.getName();
            name = name.substring(0, name.lastIndexOf('.'));
            File html = new File(directory, name + "_index.html");
            Document document = new Document();
            HtmlWriter.getInstance(document, new FileOutputStream(html));
            Object css = getValue("css");
            if (css != null) {
                document.add(new Header(HtmlTags.STYLESHEET, css.toString()));
            }
            Object title = reader.getInfo().get("Title");
            if (title == null)
                document.addTitle("Index for " + src.getName());
            else
                document.addKeywords("Index for '" + title + "'");
            Object keywords = reader.getInfo().get("Keywords");
            if (keywords != null)
                document.addKeywords((String)keywords);
            Object description = reader.getInfo().get("Subject");
            if (keywords != null)
                document.addSubject((String)description);
            document.open();
            Paragraph t;
            if (title == null)
                t = new Paragraph("Index for " + src.getName());
            else
                t = new Paragraph("Index for '" + title + "'");
            document.add(t);
            if (description != null) {
                Paragraph d = new Paragraph((String) description);
                document.add(d);
            }
            List<HashMap<String, Object>> list = SimpleBookmark.getBookmark(reader);
            if (list == null) {
                document.add(new Paragraph("This document has no bookmarks."));
            }
            else {
                for (HashMap<String, Object> c: list) {
                    Chapter chapter = (Chapter)createBookmark(src.getName(), null, c);
                    List<HashMap<String, Object>> kids = (List<HashMap<String, Object>>) c.get("Kids");
                    if (kids != null) {
                        for (HashMap<String, Object> m: kids) {
                            addBookmark(src.getName(), chapter, m);
                        }
                    }
                    document.add(chapter);
                }
            }
            document.close();
            Executable.launchBrowser(html.getAbsolutePath());
        }
        catch(Exception e) {
            e.printStackTrace();
            JOptionPane.showMessageDialog(internalFrame,
                    e.getMessage(),
                    e.getClass().getName(),
                    JOptionPane.ERROR_MESSAGE);
            System.err.println(e.getMessage());
        }
    }

    
    private static void addBookmark(String pdf, Section section, HashMap<String, Object> bookmark) {
        Section s = createBookmark(pdf, section, bookmark);
        List<HashMap<String, Object>> kids = (List<HashMap<String, Object>>) bookmark.get("Kids");
        if (kids == null) return;
        for (HashMap<String, Object> m: kids) {
            addBookmark(pdf, s, m);
        }
    }

    
    private static Section createBookmark(String pdf, Section section, HashMap<String, Object> bookmark) {
        Section s;
        Paragraph title = new Paragraph((String)bookmark.get("Title"));
        System.out.println((String)bookmark.get("Title"));
        String action = (String)bookmark.get("Action");
        if ("GoTo".equals(action)) {
            if (bookmark.get("Page") != null) {
                String page = (String)bookmark.get("Page");
                StringTokenizer tokens = new StringTokenizer(page);
                String token = tokens.nextToken();
                Anchor anchor = new Anchor(" page" + token);
                anchor.setReference(pdf + "#page=" + token);
                title.add(anchor);
            }
        }
        else if ("URI".equals(action)) {
            String url = (String)bookmark.get("URI");
            Anchor anchor = new Anchor(" Goto URL");
            anchor.setReference(url);
            title.add(anchor);
        }
        else if ("GoToR".equals(action)) {
            String remote = (String)bookmark.get("File");
            Anchor anchor = new Anchor(" goto " + remote);
            if (bookmark.get("Named") != null) {
                String named = (String)bookmark.get("Named");
                remote = remote + "#nameddest=" + named;
            }
            else if (bookmark.get("Page") != null) {
                String page = (String)bookmark.get("Page");
                StringTokenizer tokens = new StringTokenizer(page);
                String token = tokens.nextToken();
                anchor.add(new Chunk(" page " + token));
                remote = remote + "#page=" + token;
            }
            anchor.setReference(remote);
            title.add(anchor);
        }
        if (section == null) {
            s = new Chapter(title, 0);
        }
        else {
            s = section.addSection(title);
        }
        s.setNumberDepth(0);
        return s;
    }

    
    public void valueHasChanged(AbstractArgument arg) {
        if (internalFrame == null) {
            
            return;
        }
        
    }

    
    public static void main(String[] args) {
        HtmlBookmarks tool = new HtmlBookmarks();
        if (args.length < 1) {
            System.err.println(tool.getUsage());
        }
        tool.setMainArguments(args);
        tool.execute();
    }

    
    protected File getDestPathPDF() throws InstantiationException {
        throw new InstantiationException("There is no file to show.");
    }

}
