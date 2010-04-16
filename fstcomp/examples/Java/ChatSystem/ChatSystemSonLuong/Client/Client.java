

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Iterator;
import java.io.*;
import java.net.*;

import javax.swing.JOptionPane;



/**
 * simple chat client
 */
public class Client implements Runnable {
	String servAddr;
	int servPort;
	Socket s;
	
	public static void main(String[] args) throws IOException {
		Client client = new Client();
		
	}

	protected ObjectInputStream inputStream;

	protected ObjectOutputStream outputStream;

	protected Thread thread;
	public Client(boolean validate){}
	public Client() {
		getConfiguration();
		connectToServer();
		initUI();
	}
	synchronized boolean connectToServer(){
		 
		
		try
		{
			System.out.println("Connecting to " + servAddr + " (port " + servPort
					+ ")...");
			InetAddress addr = InetAddress.getByName(servAddr); 
			s = new Socket(addr, servPort);
		}
		catch( UnknownHostException e )
		{
			JOptionPane.showMessageDialog( null,"Host Not Found, Reconfigure...","Host Lookup Error",JOptionPane.ERROR_MESSAGE );
			return false;
		}
		catch( IOException e )
		{
			JOptionPane.showMessageDialog( null,"Server Not Found, Check If Server Exists...","Socket Error",JOptionPane.ERROR_MESSAGE );
			System.exit(0);
			return false;
		}

		try
		{
			this.outputStream = new ObjectOutputStream((s.getOutputStream()));
			this.inputStream = new ObjectInputStream((s.getInputStream()));
		}
		catch( IOException e )
		{
			JOptionPane.showMessageDialog( null,"Cannot Create Data Stream, Closing Client...","Data Stream Error",JOptionPane.ERROR_MESSAGE );
			try
			{
				s.close();
			}
			catch( IOException io_e )
			{}
			
			return false;
		}
		thread = new Thread(this);
		thread.start();
				
		return true;
		
	}

	/**
	 * main method. waits for incoming messages.
	 */
	public void run() {
		try {
			while (true) {
				try {
					Object msg = inputStream.readObject();
					handleIncomingMessage(msg);
				} catch (ClassNotFoundException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

			}
		} catch (IOException ex) {
			ex.printStackTrace();
		} finally {
			thread = null;
			try {
				outputStream.close();
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}
	}
	protected synchronized void getConfiguration() {
		try {
			char[] buffer = new char[255];

			FileReader configFile = new FileReader("clientConfig.cfg");

			configFile.read(buffer);
			servAddr = String.copyValueOf(buffer);
			String[] temp = servAddr.split(";");

			servAddr = temp[0];
			servPort = Integer.parseInt(temp[1]);

		} catch (FileNotFoundException fnf_e) {
			JOptionPane.showMessageDialog(null,
					"Configuration File Not Found, Using Defaults",
					"Configuration File Missing", JOptionPane.ERROR_MESSAGE);

			servPort = 1665;
			servAddr = "localhost";
		} catch (IOException io_e) {
			JOptionPane.showMessageDialog(null,
					"Error Reading Configuration File, Using Defaults",
					"Configuration Error", JOptionPane.ERROR_MESSAGE);

			servPort = 1665;
			servAddr = "localhost";
		}
	}

	/**
	 * decides what to do with incoming messages
	 * 
	 * @param msg
	 *            the message (Object) received from the sockets
	 */
	protected void handleIncomingMessage(Object msg) {
		if (msg instanceof TextMessage) {
			fireAddLine((TextMessage)msg);
		}
	}

	public void send(String line) {
		send(new TextMessage(line));
	}

	public void send(TextMessage msg) {
		try {
			outputStream.writeObject(msg);
			outputStream.flush();
		} catch (IOException ex) {
			ex.printStackTrace();
			thread.stop();
		}
	}

	/**
	 * listener-list for the observer pattern
	 */
	protected ArrayList listeners = new ArrayList();

	/**
	 * addListner method for the observer pattern
	 */
	public void addLineListener(ChatLineListener listener) {
		listeners.add(listener);
	}

	/**
	 * removeListner method for the observer pattern
	 */
	public void removeLineListener(ChatLineListener listner) {
		listeners.remove(listner);
	}

	/**
	 * fire Listner method for the observer pattern
	 */
	public void fireAddLine(TextMessage msg) {
		for (Iterator iterator = listeners.iterator(); iterator.hasNext();) {
			ChatLineListener listener = (ChatLineListener) iterator.next();
			listener.newChatLine(msg);
		}
	}

	public void stop() {
		thread.stop();
	}
	public void initUI(){
	}

}
