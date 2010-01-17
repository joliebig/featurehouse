package net.sf.jabref.external;

import net.sf.jabref.*;

import java.io.File;
import java.io.IOException;
import java.io.FilenameFilter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.*;


public class RegExpFileSearch {

    final static String EXT_MARKER = "__EXTENSION__";

    public static void main(String[] args) {
        BibtexEntry entry = new BibtexEntry(Util.createNeutralId());
        entry.setField(BibtexFields.KEY_FIELD, "raffel01");
        entry.setField("year", "2001");
        ArrayList<String> extensions = new ArrayList<String>();
        extensions.add("pdf");
        extensions.add("ps");
        extensions.add("txt");
        List<File> dirs = new ArrayList<File>();
        dirs.add(new File("/home/alver/Desktop/Tromso_2008"));
        System.out.println(findFiles(entry, extensions, dirs,
                "**/[bibtexkey].*\\\\.[extension]"));
    }

    
    public static Map<BibtexEntry, java.util.List<File>> findFilesForSet(Collection<BibtexEntry> entries,
                 Collection<String> extensions, List<File> directories, String regExp) {

        Map<BibtexEntry, java.util.List<File>> res = new HashMap<BibtexEntry, List<File>>();
        for (BibtexEntry entry : entries) {
            res.put(entry, findFiles(entry, extensions, directories, regExp));
        }
        return res;
    }

    
    public static List<File> findFiles(BibtexEntry entry, Collection<String> extensions,
                                       Collection<File> directories, String regularExpression) {

        StringBuilder sb = new StringBuilder();
        for (Iterator<String> i = extensions.iterator(); i.hasNext();) {
            sb.append(i.next());
            if (i.hasNext())
                    sb.append("|");
        }
        String extensionRegExp = "("+sb.toString()+")";

        return findFile(entry, null, directories, regularExpression, extensionRegExp, true);
    }

        
	public static List<File> findFile(BibtexEntry entry, BibtexDatabase database, Collection<File> dirs,
		String file, String extensionRegExp, boolean relative) {
        ArrayList<File> res = new ArrayList<File>();
		for (File directory : dirs) {
            List<File> tmp = findFile(entry, database, directory.getPath(), file, extensionRegExp, relative);
            if (tmp != null)
                res.addAll(tmp);
		}
		return res;
	}

    
    public static List<File> findFile(BibtexEntry entry, BibtexDatabase database, String directory,
        String file, String extensionRegExp, boolean relative) {

        List<File> res;
        File root;
        if (directory == null) {
            root = new File(".");
        } else {
            root = new File(directory);
        }
        if (!root.exists()) {
            return null;
        }
        res = findFile(entry, database, root, file, extensionRegExp);


        if (res.size() > 0) {
            for (int i=0; i<res.size(); i++)
                try {
                    
                    
                    
                    String tmp = res.get(i).getCanonicalPath().substring(root.getCanonicalPath().length());
                    if ((tmp.length() > 1) && (tmp.charAt(0) == File.separatorChar))
                        tmp = tmp.substring(1);
                    res.set(i, new File(tmp));
                    
                } catch (IOException e) {
                    e.printStackTrace();
                }
        }
        return res;
    }

    
    protected static List<File> findFile(BibtexEntry entry, BibtexDatabase database, File directory,
        String file, String extensionRegExp) {

        ArrayList<File> res = new ArrayList<File>();

        if (file.startsWith("/")) {
            directory = new File(".");
            file = file.substring(1);
        }

        
        Matcher m = Pattern.compile("([^\\\\])\\\\([^\\\\])").matcher(file);
        StringBuffer s = new StringBuffer();
        while (m.find()) {
            m.appendReplacement(s, m.group(1) + "/" + m.group(2));
        }
        m.appendTail(s);
        file = s.toString();
        String[] fileParts = file.split("/");

        if (fileParts.length == 0)
            return res;

        if (fileParts.length > 1) {

            for (int i = 0; i < fileParts.length - 1; i++) {

                String dirToProcess = fileParts[i];
                dirToProcess = Util.expandBrackets(dirToProcess, entry, database);

                if (dirToProcess.matches("^.:$")) { 
                    directory = new File(dirToProcess + "/");
                    continue;
                }
                if (dirToProcess.equals(".")) { 
                    continue;
                }
                if (dirToProcess.equals("..")) {
                    directory = new File(directory.getParent());
                    continue;
                }
                if (dirToProcess.equals("*")) { 

                    File[] subDirs = directory.listFiles();
                    if (subDirs != null) {
                        String restOfFileString = Util.join(fileParts, "/", i + 1, fileParts.length);
                        for (int sub = 0; sub < subDirs.length; sub++) {
                            if (subDirs[sub].isDirectory()) {
                                res.addAll(findFile(entry, database, subDirs[sub],
                                    restOfFileString, extensionRegExp));
                            }
                        }
                    }
                }
                
                if (dirToProcess.equals("**")) {
                    List<File> toDo = new LinkedList<File>();
                    toDo.add(directory);

                    String restOfFileString = Util.join(fileParts, "/", i + 1, fileParts.length);

                    while (!toDo.isEmpty()) {

                        
                        File[] subDirs = toDo.remove(0).listFiles();
                        if (subDirs == null) 
                            continue;

                        toDo.addAll(Arrays.asList(subDirs));

                        for (int sub = 0; sub < subDirs.length; sub++) {
                            if (!subDirs[sub].isDirectory())
                                continue;
                            res.addAll(findFile(entry, database, subDirs[sub], restOfFileString,
                                    extensionRegExp));
                        }
                    }

                }

            } 
        }

        
        String filePart = fileParts[fileParts.length-1].replaceAll("\\[extension\\]", EXT_MARKER);
        String filenameToLookFor = Util.expandBrackets(filePart, entry, database)
                .replaceAll(EXT_MARKER, extensionRegExp);
        final Pattern toMatch = Pattern.compile("^"
            + filenameToLookFor.replaceAll("\\\\\\\\", "\\\\") + "$", Pattern.CASE_INSENSITIVE);

        File[] matches = directory.listFiles(new FilenameFilter() {
            public boolean accept(File arg0, String arg1) {
                return toMatch.matcher(arg1).matches();
            }
        });
        if (matches != null && (matches.length > 0))
            for (int i = 0; i < matches.length; i++) {
                File match = matches[i];
                res.add(match);
            }
        return res;
    }




}
