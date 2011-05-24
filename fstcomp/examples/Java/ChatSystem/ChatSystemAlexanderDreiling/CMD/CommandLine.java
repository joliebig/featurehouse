


import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class CommandLine implements Runnable, ChatLineListener{

		
		private Client chatClient;
		protected Thread thread;
		
		
		public CommandLine(Client chatClient){

			System.out.println("starting chat commandline...");
				
			// register listener so that we are informed whenever a new chat message
			// is received (observer pattern)
			chatClient.addLineListener(this);
			this.chatClient = chatClient;
		
			thread = new Thread(this);
			thread.start();
					
		}
		
		public void newChatLine(TextMessage msg) {
			String content = msg.getSender()+ ">"+ msg.getSettings() + ">" +  msg.getContent()+"\n";
			System.out.println(content);			
		}

		public void run() {
			try {
				TextMessage msg;
				String settings, input;
				String[] splitInput;
				
				while (true) {
					input = readString();
					splitInput = input.split("~");
					
					if (splitInput.length > 1) {
						msg = new TextMessage(splitInput[1]);
						msg.setSetting(splitInput[0]);
					}
					else {
						msg = new TextMessage(splitInput[0]);
							
					}
					chatClient.send(msg);

				}
			} catch (Exception ex) {
				ex.printStackTrace();
			} finally {
				thread = null;
			}
		}

		public String readString() {
			String result = null;
			BufferedReader reader = new BufferedReader(new InputStreamReader(
					System.in));
			try {
				result = reader.readLine();
			} catch (IOException e) {
				System.err.println("I/O Error: " + e.getMessage());
			}
			return result;
		}
	}