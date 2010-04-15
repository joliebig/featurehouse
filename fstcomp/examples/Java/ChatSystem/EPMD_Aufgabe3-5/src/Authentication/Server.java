

import java.io.*;
import java.net.*;
import java.util.*;
import javax.swing.*;
import java.sql.*;

public class Server {
	Connection conn = null;
	Hashtable userData = null;
	public static void main(String args[]) throws IOException {
		boolean authentication = true;
		new Server(authentication);
	}
	protected void getDB(){
		userData = new Hashtable();
		try{
			FileInputStream fstream = new FileInputStream("user.db");
		    DataInputStream in = new DataInputStream(fstream);
		    BufferedReader br = new BufferedReader(new InputStreamReader(in));
		    String strLine;
		    while ((strLine = br.readLine()) != null)   {
		    	String[] temp = strLine.split(";");
		    	userData.put(temp[0],temp[1]);
		    }
		    in.close();
		    /*Enumeration keys =  userData.keys();
		    while (keys.hasMoreElements()){
		    	System.out.println((String)(keys.nextElement()));
		    }*/
		} catch (Exception e){//Catch exception if any
		    	System.err.println("Error: " + e.getMessage());
		}
	}
	Server(boolean validate) {
		getConfiguration();
		getDB();
		try{
			ServerSocket server = new ServerSocket(serverPort);
			while (true) {
				System.out.println("Waiting for Connections...");
				Socket client = server.accept();
				System.out.println("Accepted from " + client.getInetAddress());
				Connection c = connectTo(client);
				c.start();
			}
		} catch (Exception e){
			e.printStackTrace();
		}
	}
}