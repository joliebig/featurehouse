

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Iterator;

public class Client implements Runnable{
  	protected ObjectInputStream inputStream;
    protected ObjectOutputStream outputStream;
    protected Thread thread;

	public static void main(String args[]) throws IOException {
   		if (args.length != 2) {
   			throw new RuntimeException("Syntax: ChatClient <host> <port>");
    	}
		Client client = new Client(args[0], Integer.parseInt(args[1]));        
 		new UserInterface(client);
	}
	
	public Client(String host, int port) {
    	try {
        	System.out.println("Connecting to " + host + " (port " + port + ")...");
            Socket s = new Socket(host, port);
            this.outputStream = new ObjectOutputStream((s.getOutputStream()));
            this.inputStream = new ObjectInputStream((s.getInputStream()));
            thread = new Thread(this);
            thread.start();
        } catch (Exception e) {
            e.printStackTrace();
        }
      }
    /**
     * main method. waits for incoming messages.
     */
    public void run() {
        init();
        try {	
        	while (true) {
                try {
                    Object msg = inputStream.readObject();
                    //Ueberpruefung ob eine TextMessage empfangen wurde
                    if(msg instanceof TextMessage)
                    	handleIncomingMessage((TextMessage) msg);
                } catch (ClassNotFoundException e) {
            		e.printStackTrace();
                }
            }
        } catch (IOException ex) {
            ex.printStackTrace();
            System.exit(0);
        } finally {
            thread = null;
            try {
                outputStream.close();
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
    }
// Inhalt der Nachricht wird an die Listener weiter geleitet
 	public void handleIncomingMessage(TextMessage msg) {      
	 String line = ((TextMessage) msg).getContent();
     fireAddLine(line + "\n");	 	
    
    }	
    
    // Neue Textnachricht wird erzeugt
    public void send(String line) {
    	send(new TextMessage(line));
    }
    
    // Nachricht wird versendet
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
    private ArrayList listeners = new ArrayList();

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
    public void fireAddLine(String line) {
        for (Iterator iterator = listeners.iterator(); iterator.hasNext();) {
            ChatLineListener listener = (ChatLineListener) iterator.next();
            listener.newChatLine(line);
        }
    }

    public void stop() {
        thread.stop();
    }
    public void init(){
    }
    
}