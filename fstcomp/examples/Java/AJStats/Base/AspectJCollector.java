import java.io.*;
import java.util.*;
import ajparser.*;

class AspectJCollector extends AspectJParser {
    
    protected Statistics stats = new Statistics();
    static protected int failures = 0;
    
    AspectJCollector(InputStream in) {
    	super(in);
    }
 
    public boolean parseFile(String filename) throws FileNotFoundException, IOException {
        return parseString(loadFile(filename));
    }
    
    private StringBuffer loadFile(String fileName) throws FileNotFoundException, IOException {
        StringBuffer fileContent = new StringBuffer();
        FileReader f = new FileReader(fileName);
        char[] buf = new char[1000];
    
        int sz = 0;
        int l = 0;
        while ( (sz = f.read(buf)) > 0)
        {
            fileContent.append(buf);
            l += sz;
        }
    
        fileContent.setLength(l);
        return fileContent;
    }

    
  
	boolean parseString(StringBuffer buffer) {
    	StringReader reader = new StringReader(buffer.toString());
		System.out.println(buffer.toString());
	    ReInit(reader);
        try {
            long startTime = System.currentTimeMillis();
            CompilationUnit();
            long parseTime = System.currentTimeMillis() - startTime;
            return true;
        } catch (ParseException e) {
            System.out.println(e.getMessage());
            return false;
        }
    }

    public static void main (String[] args) {
        AspectJCollector parser = new AspectJCollector(System.in);
        if (args.length == 0)  {
            System.out.println(parserName + ":  Reading from standard input . . .");
            try {
                parser.CompilationUnit();
            } catch (ParseException e) {
                System.out.println(e.getMessage());
                System.out.println(parserName + ":  Encountered errors during parse.");
            }
        } else {
            for(int i = 0; i < args.length; i++) {
                try {
					System.out.println("looking for files...");
                	Vector files = parser.getFileNames(new File(args[i]), null);
                	for(int j = 0; j < files.size(); j++) {
                        System.out.println("analyzing file: " + ((File)files.get(j)).getName());
                		if(!parser.parseFile(((File)files.get(j)).getPath())) {
                            System.out.println("failure parsing " + ((File)files.get(j)).getPath() + "!");
                            failures++;
                		} else {
                			parser.stats.file_count++;
                		}
                    }
                } catch(FileNotFoundException e) {
                    System.out.println("*** File not found: " + args[i] + ":\n" + e);
                } catch(IOException e) {
                    System.out.println("*** IO error: " + args[i] + ":\n" + e);
                }
            }
        }
        parser.printStatistics();
        System.out.println("===");
        if(failures == 0)
            System.out.println("all files have been parsed");
        else
            System.out.println("" + failures + " file(s) could not been parsed");
    }

    protected Vector getFileNames(File file, Vector vec) {
    	if(vec == null)
    		vec = new Vector();
    	
    	if(file.isFile()) {
    		vec.add(file);
        	return vec;    		
    	} else {
            System.out.println("including directory: " + file.getPath());
    		File[] files = file.listFiles(new AspectJFileFilter());
    		
    		if(files != null) {
    			for(int i = 0; i < files.length; i++) {
    				vec = getFileNames(files[i], vec);
    			}
    		}
    	}
    	return vec;
    }
    
    protected void printStatistics() {
        stats.print();
    }
}
