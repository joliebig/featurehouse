

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JComponent;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;

/**
 * simple AWT gui for the chat client
 */
public class Gui extends JFrame implements ChatLineListener {

	private static final long serialVersionUID = 1L;

	protected JFrame window;
	
	protected JScrollPane scrollPane;
	protected JTextPane outputTextbox;
	protected JTextField inputField;

	protected Client chatClient;

	/**
	 * creates layout
	 * 
	 * @param title
	 *            title of the window
	 * @param chatClient
	 *            chatClient that is used for sending and receiving messages
	 */
	public Gui(String title, final Client chatClient) {
		super(title);
		
		window = this;
		
		System.out.println("starting gui...");
		setLayout(new GridBagLayout());
		
		outputTextbox = new JTextPane();
		outputTextbox.setEditable(false);
		
		scrollPane = new JScrollPane();
		scrollPane.setViewportView(outputTextbox);
		addScrollPane(this, scrollPane);
		
		inputField = new JTextField();
		addTextField(this, inputField);
		
		this.setDefaultCloseOperation(EXIT_ON_CLOSE);
		
		pack();
		setSize(400, 300);
		setVisible(true);		
		
		inputField.requestFocus();

		this.chatClient = chatClient;
		
		this.addWindowListener(new WindowListener() {
			
			public void windowOpened(WindowEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			public void windowIconified(WindowEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			public void windowDeiconified(WindowEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			public void windowDeactivated(WindowEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			public void windowClosing(WindowEvent e) {
				chatClient.stop();
				setVisible(false);
				
				System.exit(0);
				
			}
			
			public void windowClosed(WindowEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			public void windowActivated(WindowEvent e) {
				// TODO Auto-generated method stub
				
			}
		});
		
		// register listener so that we are informed whenever a new chat message
		// is received (observer pattern)
		chatClient.addLineListener(this);
		chatClient.start();
	}
	
	protected void addScrollPane(JFrame frame, JScrollPane scrollPane) {
		GridBagConstraints c = new GridBagConstraints();
		
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 1;
		frame.add(scrollPane, c);	
	}
	
	protected void addTextField(JFrame frame, final JTextField inputField) {
		GridBagConstraints c = new GridBagConstraints();
		
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 0.8;
		c.weighty = 0;
		c.gridx = 0;
		c.gridy = 1;
		frame.add(inputField, c);
		
		inputField.addKeyListener(new KeyListener() {
			
			public void keyTyped(KeyEvent arg0) {
				// TODO Auto-generated method stub
				
			}
			
			public void keyReleased(KeyEvent arg0) {
				// TODO Auto-generated method stub
				
			}
			
			public void keyPressed(KeyEvent arg0) {
				if (arg0.getKeyCode() == KeyEvent.VK_ENTER
						&& inputField.getText().length() > 0) {
					chatClient.send(inputField.getText());
					inputField.setText("");
				}
				
			}
		});
	}

	/**
	 * this method gets called every time a new message is received (observer
	 * pattern)
	 */
	public void newChatLine(String line) {
		StyleContext sc = StyleContext.getDefaultStyleContext();
	    AttributeSet aset = sc.addAttribute(SimpleAttributeSet.EMPTY,
	        StyleConstants.Foreground, Color.BLACK);
		
		try {
			outputTextbox.getDocument().insertString(outputTextbox.getDocument().getLength(),
													 line, aset);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}
}
