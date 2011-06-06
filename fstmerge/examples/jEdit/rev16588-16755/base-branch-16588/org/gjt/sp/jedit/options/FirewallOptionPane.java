

package org.gjt.sp.jedit.options;

 
import java.awt.event.*;
import javax.swing.*;
import org.gjt.sp.jedit.*;


public class FirewallOptionPane extends AbstractOptionPane {

	
	public FirewallOptionPane()
	{
		super("firewall");
	} 

	
	public void _init()
	{
		
		addComponent(httpEnabled = new JCheckBox(jEdit.getProperty(
			"options.firewall.http.enabled")));
		
		addComponent(jEdit.getProperty("options.firewall.http.host"), 
			httpHost = new JTextField(jEdit.getProperty("firewall.host"), 15));
		
		addComponent(jEdit.getProperty("options.firewall.http.port"), 
			httpPort = new JTextField(jEdit.getProperty("firewall.port"), 15));
		
		addComponent(jEdit.getProperty("options.firewall.http.user"),
			httpUser = new JTextField(jEdit.getProperty("firewall.user"), 15));
		
		addComponent(jEdit.getProperty("options.firewall.http.password"),
			httpPass = new JPasswordField(jEdit.getProperty("firewall.password"), 15));
		
		addComponent(jEdit.getProperty("options.firewall.http.nonProxy"),
			httpNonProxy = new JTextField(jEdit.getProperty("firewall.nonProxyHosts"), 15));

		boolean enabled = jEdit.getBooleanProperty("firewall.enabled");
		httpEnabled.setSelected(enabled);
		httpHost.setEnabled(enabled);
		httpPort.setEnabled(enabled);
		httpUser.setEnabled(enabled);
		httpPass.setEnabled(enabled);
		httpNonProxy.setEnabled(enabled);

		httpEnabled.addActionListener(new ActionHandler());

		
		addComponent(socksEnabled = new JCheckBox(jEdit.getProperty(
			"options.firewall.socks.enabled")));
		
		addComponent(jEdit.getProperty("options.firewall.socks.host"), 
			socksHost = new JTextField(jEdit.getProperty("firewall.socks.host"), 15));
		
		addComponent(jEdit.getProperty("options.firewall.socks.port"), 
			socksPort = new JTextField(jEdit.getProperty("firewall.socks.port"), 15));

		enabled = jEdit.getBooleanProperty("firewall.socks.enabled");
		socksEnabled.setSelected(enabled);
		socksHost.setEnabled(enabled);
		socksPort.setEnabled(enabled);

		socksEnabled.addActionListener(new ActionHandler());
	} 

	
	public void _save()
	{
		jEdit.setBooleanProperty("firewall.enabled", httpEnabled.isSelected());
		jEdit.setProperty("firewall.host", httpHost.getText());
		jEdit.setProperty("firewall.port", httpPort.getText());
		jEdit.setProperty("firewall.user", httpUser.getText());
		jEdit.setProperty("firewall.password", new String(httpPass.getPassword()));
		jEdit.setProperty("firewall.nonProxyHosts", httpNonProxy.getText());

		jEdit.setBooleanProperty("firewall.socks.enabled", socksEnabled.isSelected());
		jEdit.setProperty("firewall.socks.host", socksHost.getText());
		jEdit.setProperty("firewall.socks.port", socksPort.getText());
	} 

	
	private JCheckBox httpEnabled;
	private JTextField httpHost;
	private JTextField httpPort;
	private JTextField httpUser;
	private JPasswordField httpPass;
	private JTextField httpNonProxy;
	private JCheckBox socksEnabled;
	private JTextField socksHost;
	private JTextField socksPort;
	

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			httpHost.setEnabled(httpEnabled.isSelected());
			httpPort.setEnabled(httpEnabled.isSelected());
			httpUser.setEnabled(httpEnabled.isSelected());
			httpPass.setEnabled(httpEnabled.isSelected());
			httpNonProxy.setEnabled(httpEnabled.isSelected());
			socksHost.setEnabled(socksEnabled.isSelected());
			socksPort.setEnabled(socksEnabled.isSelected());
		}
	} 
}
