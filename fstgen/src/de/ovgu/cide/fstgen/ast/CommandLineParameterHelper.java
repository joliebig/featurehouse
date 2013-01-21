package de.ovgu.cide.fstgen.ast;

public class CommandLineParameterHelper {

	private static boolean isJML=false;
	
	public static void setJML(boolean status){
		isJML = status;
	}
	
	public static boolean isJML(){
		return isJML;
	}
	
	
}
