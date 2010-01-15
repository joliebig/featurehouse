package merger;

import java.io.BufferedReader;
import java.io.BufferedWriter;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class LineBasedMerger implements MergerInterface {
	
	String encoding = "UTF-8";

	public void merge(FSTTerminal node) throws ContentMergeException {
		
		if(!(node.getType().equals("MethodDecl") || node.getType().contains("-Content")))
		//if(!node.getType().equals("MethodDecl"))
			return;
		
		//System.err.println(node.getBody());
		
		String[] tokens = node.getBody().split(FSTGenMerger.MERGE_SEPARATOR);
		
		//StringTokenizer st = new StringTokenizer(node.getBody(), FSTGenMerger.MERGE_SEPARATOR);
		
		//if((st.countTokens() < 2))
		if(tokens.length < 2)
			throw new ContentMergeException(node.getBody());
		
	    try {
	    	File tmpDir = new File(System.getProperty("user.dir") + File.separator + "fstmerge_tmp");
	    	tmpDir.mkdir();
	    	
			File fileVar1 = File.createTempFile("fstmerge_var1_", "", tmpDir);
			File fileBase = File.createTempFile("fstmerge_base_", "", tmpDir);
			File fileVar2 = File.createTempFile("fstmerge_var2_", "", tmpDir);
			
	    	BufferedWriter writerVar1 = new BufferedWriter(new FileWriter(fileVar1));
	        //writerVar1.write(st.nextToken().replaceAll(FSTGenMerger.SEMANTIC_MERGE_MARKER, ""));
	    	writerVar1.write(tokens[0].replaceAll(FSTGenMerger.SEMANTIC_MERGE_MARKER, ""));
	        writerVar1.close();
	        BufferedWriter writerBase = new BufferedWriter(new FileWriter(fileBase));
	        //writerBase.write(st.nextToken());
	        writerBase.write(tokens[1]);
	        writerBase.close();
	        BufferedWriter writerVar2 = new BufferedWriter(new FileWriter(fileVar2));
	        
	        //if(st.hasMoreTokens()) writerVar2.write(st.nextToken());
	        if(tokens.length > 2) writerVar2.write(tokens[2]);
	        else writerVar2.write("");
	        writerVar2.close();

	        
	        String mergeCmd = "C:\\Programme\\cygwin\\bin\\merge.exe -q -p " + "\"" + fileVar1.getPath() + "\"" + " " + "\"" + fileBase.getPath() + "\"" + " " + "\"" + fileVar2.getPath() + "\"";// + " > " + fileVar1.getName() + "_output";
	        //System.err.println(mergeCmd);
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
	    
 
		
		
/*		
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		ByteArrayInputStream isVar1 = new ByteArrayInputStream(st.nextToken().trim().getBytes());
		ByteArrayInputStream isBase = new ByteArrayInputStream(st.nextToken().trim().getBytes());
		ByteArrayInputStream isVar2;
		if(st.hasMoreTokens())
			isVar2 = new ByteArrayInputStream(st.nextToken().trim().getBytes());
		else
			isVar2 = new ByteArrayInputStream(new String("").getBytes());
		
		
		
		IStatus stat = textMerger.merge(os, encoding, isBase, encoding, isVar1, encoding, isVar2, encoding, new NullProgressMonitor());

		if(!stat.isOK()) {
			System.err.println(stat.getMessage());
		}
		else
			System.out.println(os.toString());
*/
	}
}
