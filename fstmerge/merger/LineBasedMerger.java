package merger;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;


import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class LineBasedMerger implements MergerInterface {
	
	String encoding = "UTF-8";

	public void merge(FSTTerminal node) throws ContentMergeException {
		
		String body = node.getBody() + " ";
		String[] tokens = body.split(FSTGenMerger.MERGE_SEPARATOR);
		
		try {
			tokens[0] = tokens[0].replace(FSTGenMerger.SEMANTIC_MERGE_MARKER, "").trim();
			tokens[1] = tokens[1].trim();
			tokens[2] = tokens[2].trim();
		} catch (ArrayIndexOutOfBoundsException e) {
			System.err.println("|"+body+"|");
			e.printStackTrace();
		}

		//System.out.println("|" + tokens[0] + "|");
		//System.out.println("|" + tokens[1] + "|");
		//System.out.println("|" + tokens[2] + "|");
		//System.out.println("--------------------");

		// SPECIAL CONFLICT HANDLER
		if(!(node.getType().contains("-Content") ||
			node.getMergingMechanism().equals("LineBased")
		)) {
			if(tokens[0].length() == 0 && tokens[1].length() == 0 && tokens[2].length() == 0) {
				node.setBody("");
			} else if(tokens[0].equals(tokens[2])) {
				node.setBody(tokens[0]);
			} else if(tokens[0].equals(tokens[1]) && tokens[2].length() > 0) {
				node.setBody(tokens[2]);
			} else if(tokens[2].equals(tokens[1]) && tokens[0].length() > 0) {
				node.setBody(tokens[0]);
			} else if(tokens[0].equals(tokens[1]) && tokens[2].length() == 0) {
				node.setBody("");
			} else if(tokens[2].equals(tokens[1]) && tokens[0].length() == 0) {
				node.setBody("");
			}
			//System.out.println(node.getMergingMechanism());
			//System.out.println("|" + tokens[1] + "|");
			//System.out.println("|" + tokens[2] + "|");
			//System.out.println("--------------------");
			
			
			return;
		}

	    try {
	    	long time = System.currentTimeMillis();
	    	File tmpDir = new File(System.getProperty("user.dir") + File.separator + "fstmerge_tmp"+time);
	    	tmpDir.mkdir();
	    	
			File fileVar1 = File.createTempFile("fstmerge_var1_", "", tmpDir);
			File fileBase = File.createTempFile("fstmerge_base_", "", tmpDir);
			File fileVar2 = File.createTempFile("fstmerge_var2_", "", tmpDir);
			
			BufferedWriter writerVar1 = new BufferedWriter(new FileWriter(fileVar1));
	        if(node.getType().contains("-Content") || tokens[0].length() == 0)
	        	writerVar1.write(tokens[0]);
	        else 
	        	writerVar1.write(tokens[0] + "\n");
	        writerVar1.close();
	        
	        BufferedWriter writerBase = new BufferedWriter(new FileWriter(fileBase));
	        if(node.getType().contains("-Content") || tokens[1].length() == 0)
	        	writerBase.write(tokens[1]);
	        else 
	        	writerBase.write(tokens[1] + "\n");
	        writerBase.close();

	        BufferedWriter writerVar2 = new BufferedWriter(new FileWriter(fileVar2));
	        if(node.getType().contains("-Content") || tokens[2].length() == 0)
	        	writerVar2.write(tokens[2]);
	        else 
	        	writerVar2.write(tokens[2] + "\n");
	        writerVar2.close();

	        String mergeCmd = ""; 
	        if(System.getProperty("os.name").contains("Windows"))
	        	mergeCmd = "C:\\Programme\\cygwin\\bin\\merge.exe -q -p " + "\"" + fileVar1.getPath() + "\"" + " " + "\"" + fileBase.getPath() + "\"" + " " + "\"" + fileVar2.getPath() + "\"";// + " > " + fileVar1.getName() + "_output";
	        else
	        	mergeCmd = "merge -q -p " + fileVar1.getPath() + " " + fileBase.getPath() + " " + fileVar2.getPath();// + " > " + fileVar1.getName() + "_output";
	        Runtime run = Runtime.getRuntime();
			Process pr = run.exec(mergeCmd);

			BufferedReader buf = new BufferedReader(new InputStreamReader(pr.getInputStream()));
			String line = "";
			String res = "";
			while ((line=buf.readLine())!=null) {
				res += line + "\n";
			}
			pr.getInputStream().close();
			node.setBody(res);
			
			buf = new BufferedReader(new InputStreamReader(pr.getErrorStream()));
			while ((line=buf.readLine())!=null) {
				System.err.println(line);
			}
			pr.getErrorStream().close();
			pr.getOutputStream().close();
	        
		    fileVar1.delete();
		    fileBase.delete();
		    fileVar2.delete();
		    tmpDir.delete();

	    } catch (IOException e) {
	    	e.printStackTrace();
	    }
	}
}
