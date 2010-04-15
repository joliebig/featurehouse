

public class ClientGUI {
	private JCheckBox encryption1Box;
	public void initGUI() throws Exception {
		original();
		final TransportEncryption enc1 = EncryptionFactory.getFactory().getEncryption("reverse");
		encryption1Box = new JCheckBox("Encryption " + enc1.getName());
		encryption1Box.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				try {
				if (encryption1Box.isSelected())
					synchronized(client) {
						client.addEncryption(enc1);
					}
				else
					synchronized(client) {
						client.removeEncryption(enc1);
					}
				} catch (IOException ex) {
					ex.printStackTrace();
				}
			}
			
		});
		
		add(encryption1Box);
	}
}