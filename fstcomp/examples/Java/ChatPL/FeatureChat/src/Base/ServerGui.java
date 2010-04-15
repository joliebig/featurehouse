

import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.IOException;

import javax.swing.*;

public class ServerGui extends JFrame implements ActionListener {
	JButton startButton;
	JTextField portText;
	JLabel portLabel;
	Container pane;

	public  ServerGui() 
	{
				
		pane = this.getContentPane();
		pane.setLayout(new GridLayout( 5, 2, 1, 1 ) );
		
		add();
		
		this.setTitle("Server"); 
	    this.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE ); 
	    this.setSize( 330, 200 );
	    this.setVisible( true );
	    
	}
	
	public void add()
	{
		portLabel = new JLabel();
		portLabel.setText("Port: ");
		
		portText = new JTextField();
		portText.setText("8080");
				
		pane.add(portLabel);
		pane.add(portText);
		
		startButton = new JButton();
		startButton.setText("Start");
		startButton.addActionListener(this);
		
		pane.add(startButton);
	}

	//@Override
	public void actionPerformed(ActionEvent e) {

		Object object = e.getSource();

		if (object == startButton) {
			
		try {
				this.setVisible( false );
				System.out.println("Server started...");
				Server server = new Server(Integer.parseInt(portText.getText()),"");
			} catch (NumberFormatException e1) {
				e1.printStackTrace();
			} catch (IOException e1) {
				e1.printStackTrace();
			}
			

		}

	}

	

}