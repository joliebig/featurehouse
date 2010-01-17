package net.sf.jabref.export; 

import sun.misc.CharacterEncoder; 

import java.io.*; 
import java.nio.charset.Charset; 
import java.nio.charset.CharsetEncoder; 
import java.util.TreeSet; 
import java.util.Iterator; 

import net.sf.jabref.Globals; 

import java.io.IOException; 
import java.io.OutputStream; 
import java.io.OutputStreamWriter; 
import java.io.UnsupportedEncodingException; 


public  class  VerifyingWriter  extends OutputStreamWriter {
	

	CharsetEncoder encoder;

	
	private boolean couldEncodeAll = true;

	
    

	

	public VerifyingWriter(OutputStream out, String encoding)
			throws UnsupportedEncodingException {
		super(out, encoding);
		encoder = Charset.forName(encoding).newEncoder();
	}


	

	public void write(String str) throws IOException {
		super.write(str);
		if (!encoder.canEncode(str)) {
			for (int i = 0; i < str.length(); i++) {
				if (!encoder.canEncode(str.charAt(i)))
					problemCharacters.add(new Character(str.charAt(i)));
			}
			couldEncodeAll = false;
		}
	}


	

	public boolean couldEncodeAll() {
		return couldEncodeAll;
	}


	

	public String getProblemCharacters() {
		StringBuffer chars = new StringBuffer();
		for (Character ch : problemCharacters) {
			chars.append(ch.charValue());
		}
		return chars.toString();
	}


	
	private TreeSet<Character> problemCharacters = new TreeSet<Character>();


}
