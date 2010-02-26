package merger;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.StreamTokenizer;

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

		//System.err.println("|" + tokens[0] + "|");
		//System.err.println("|" + tokens[1] + "|");
		//System.err.println("|" + tokens[2] + "|");


		if(!(node.getType().equals("MethodDecl") || 
			node.getType().equals("ConstructorDecl") || 
			node.getType().contains("-Content"))) {
			if(tokens[0].length() == 0 && tokens[1].length() == 0 && tokens[2].length() == 0) {
				node.setBody("");
			} else if(tokens[0].equals(tokens[2])) {
				node.setBody(tokens[0]);
			} else if(tokens[0].equals(tokens[1]) && tokens[2].length() > 0) {
				node.setBody(tokens[2]);
			} else if(tokens[2].equals(tokens[1]) && tokens[0].length() > 0) {
				node.setBody(tokens[1]);
			} else if(tokens[0].equals(tokens[1]) && tokens[2].length() == 0) {
				node.setBody("");
			} else if(tokens[2].equals(tokens[1]) && tokens[0].length() == 0) {
				node.setBody("");
			}
			return;
		}
		
		
	    try {
	    	File tmpDir = new File(System.getProperty("user.dir") + File.separator + "fstmerge_tmp");
	    	tmpDir.mkdir();
	    	
			File fileVar1 = File.createTempFile("fstmerge_var1_", "", tmpDir);
			File fileBase = File.createTempFile("fstmerge_base_", "", tmpDir);
			File fileVar2 = File.createTempFile("fstmerge_var2_", "", tmpDir);
			
	    	BufferedWriter writerVar1 = new BufferedWriter(new FileWriter(fileVar1));
	        if(node.getType().contains("-Content"))
	        	writerVar1.write(tokens[0]);
	        else 
	        	writerVar1.write(tokens[0] + "\n");
	        writerVar1.close();
	        
	        BufferedWriter writerBase = new BufferedWriter(new FileWriter(fileBase));
	        if(node.getType().contains("-Content"))
	        	writerBase.write(tokens[1]);
	        else 
	        	writerBase.write(tokens[1] + "\n");
	        writerBase.close();

	        BufferedWriter writerVar2 = new BufferedWriter(new FileWriter(fileVar2));
	        if(node.getType().contains("-Content"))
	        	writerVar2.write(tokens[2]);
	        else 
	        	writerVar2.write(tokens[2] + "\n");
	        writerVar2.close();

	        //if(node.getType().contains("-Content")) {
	        //	removeComments(fileVar1);
	        //	removeComments(fileBase);
	        //	removeComments(fileVar2);
	        //}

	        String mergeCmd = ""; 
	        if(System.getProperty("os.name").contains("Windows"))
	        	mergeCmd = "C:\\Programme\\cygwin\\bin\\merge.exe -q -p " + "\"" + fileVar1.getPath() + "\"" + " " + "\"" + fileBase.getPath() + "\"" + " " + "\"" + fileVar2.getPath() + "\"";// + " > " + fileVar1.getName() + "_output";
	        else
	        	mergeCmd = "merge -q -p " + fileVar1.getPath() + " " + fileBase.getPath() + " " + fileVar2.getPath();// + " > " + fileVar1.getName() + "_output";
	        Runtime run = Runtime.getRuntime();
			Process pr = run.exec(mergeCmd);
			//pr.waitFor();
			
			BufferedReader buf = new BufferedReader(new InputStreamReader(pr.getInputStream()));
			String line = "";
			String res = "";
			while ((line=buf.readLine())!=null) {
				res += line + "\n";
			}
			node.setBody(res);
			
			buf = new BufferedReader(new InputStreamReader(pr.getErrorStream()));
			while ((line=buf.readLine())!=null) {
				System.err.println(line);
			}
	        
		    fileVar1.delete();
		    fileBase.delete();
		    fileVar2.delete();
		    tmpDir.delete();

	    } catch (IOException e) {
	    	e.printStackTrace();
	    } //catch (InterruptedException e) {
	    	//e.printStackTrace();
	    //}
	}
/*	
	private void removeComments(File file) throws IOException {
		
		String tmpFileName = "FSTMerge__cpp";
		
		
		File workDir = new File("c:/Programme/cygwin/bin");
		String[] cmdArray = new String[5];
		cmdArray[0] = "c:/Programme/cygwin/bin/bash";
		cmdArray[1] = "--login";
		cmdArray[2] = "-i";
		cmdArray[3] = "-c";
		cmdArray[4] = "cpp -P " + "\"" + file.getPath() + "\"" + " -o " + tmpFileName;
		System.err.println(cmdArray[4]);
		Process pr = Runtime.getRuntime().exec(cmdArray, null, workDir);
		
		File tmpFile = new File(tmpFileName);
		
		FileReader in = new FileReader(tmpFile);
	    FileWriter out = new FileWriter(file);
	    int c;

	    while ((c = in.read()) != -1)
	      out.write(c);

	    in.close();
	    out.close();
		
	}
	*/
}
