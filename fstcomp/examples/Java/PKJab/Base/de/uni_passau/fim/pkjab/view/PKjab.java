
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Iterator;
import java.util.TreeSet;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.BevelBorder;

import pkjab.de.uni_passau.fim.pkjab.model.Connection;
import pkjab.de.uni_passau.fim.pkjab.model.Contact;
import pkjab.de.uni_passau.fim.pkjab.model.messages.Message;
import pkjab.de.uni_passau.fim.pkjab.util.Jid;
import pkjab.de.uni_passau.fim.pkjab.util.Observer;
import pkjab.de.uni_passau.fim.pkjab.util.UserState;

public class PKjab extends JFrame implements Observer {
	
	private static final String APP_NAME = "PKjab";

	private Connection connection;
	
	private final String user;

	private JPanel viewport;

	private final JPanel bottomPanel = new JPanel();

	private JComboBox stateCombo;

	private JButton menuButton;
	
	private static final String ICON_ONLINE = PKjabToolkit.ICON_DIR + "user_online.png";
	
	private static final String ICON_OFFLINE = PKjabToolkit.ICON_DIR + "user_offline.png";
	
	private static final String ICON_AWAY = PKjabToolkit.ICON_DIR + "user_away.png";
	
	private static final String ICON_XA = PKjabToolkit.ICON_DIR + "user_xa.png";
	
	private static final String ICON_DND = PKjabToolkit.ICON_DIR + "user_dnd.png";
	
	private static final String ICON_CHAT = PKjabToolkit.ICON_DIR + "user_online.png";
	
	private static final String INPUT_JID = "Geben Sie die JID ein: ";
	
	private static final String ENTER_USERDATA = "Benutzerdaten";

	private static final String CMD_ADDCONTACT = "<html>Kontakt hinzuf&uuml;gen</html>";
	
	private static final String MENU_BUTTON = "<html>Men&uuml;</html>";

	private static final String CMD_EXIT = "<html>Beenden</html>";
	
	protected final TreeSet contactPanels = new TreeSet();

	public PKjab(final String user, final String domain, final String password) {
		super(APP_NAME + ": " + user);
		
		connection = new Connection(new Jid(user, domain, APP_NAME), password);
		this.user = user;
		
		readStatusIcons();
		setupGui();
		update((Message) null);
		
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setPreferredSize(new Dimension(200, 400));
		pack();
		setVisible(true);
		
		connection.attach(this);
	}
	
	private void readStatusIcons() {
		HashMap statusIcons = PKjabToolkit.getStatusIcons();
		
		statusIcons.put(UserState.ONLINE, PKjabToolkit.getImageIcon(ICON_ONLINE));
		statusIcons.put(UserState.OFFLINE, PKjabToolkit.getImageIcon(ICON_OFFLINE));
		statusIcons.put(UserState.AWAY, PKjabToolkit.getImageIcon(ICON_AWAY));
		statusIcons.put(UserState.XA, PKjabToolkit.getImageIcon(ICON_XA));
		statusIcons.put(UserState.DND, PKjabToolkit.getImageIcon(ICON_DND));
		statusIcons.put(UserState.CHAT, PKjabToolkit.getImageIcon(ICON_CHAT));
	}
	
	private void setupGui() {
		
		JPanel contentPane = new JPanel();
		contentPane.setLayout(new BorderLayout());
		setContentPane(contentPane);
		
		createStateCombo();
		createMenu();
		createScrollPane();
		createBottomPanel();
		
		getContentPane().add(bottomPanel, BorderLayout.PAGE_END);
	}
	
	private void createStateCombo() {
		UserState[] items = new UserState[6];
		items[0] = UserState.ONLINE;
		items[1] = UserState.AWAY;
		items[2] = UserState.XA;
		items[3] = UserState.DND;
		items[4] = UserState.CHAT;
		items[5] = UserState.OFFLINE;
		
		stateCombo = new JComboBox(items);
		stateCombo.setSelectedItem(connection.getUserState());
		stateCombo.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				connection.setUserState((UserState) stateCombo.getSelectedItem(), null);
			}
			
		});
	}
	
	private void createMenu() {
		
		final JPopupMenu menu = new JPopupMenu();
		
		JMenuItem item;
		
/*		item = new JMenuItem(CMD_ADDCONTACT);
		item.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				String jid = showInputDialog(INPUT_JID, CMD_ADDCONTACT);
				System.out.println(jid);
			}
		});
		menu.add(item);
		
		menu.addSeparator();
*/		
		item = new JMenuItem(CMD_EXIT);
		item.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				connection.disconnect();
				System.exit(0);
			}
		});
		menu.add(item);
		
		menuButton = new JButton(MENU_BUTTON);
		
		menuButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				menu.show(menuButton, 0, menuButton.getHeight());
			}
		});
		
	}
	
	private void createScrollPane() {
		
		viewport = new JPanel();
		viewport.setLayout(new BoxLayout(viewport, BoxLayout.Y_AXIS));

		getContentPane().add(
				new JScrollPane(viewport,
						ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
						ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER),
				BorderLayout.PAGE_START);
	}
	
	private void createBottomPanel() {
		bottomPanel.setLayout(new BorderLayout());
		bottomPanel.setBorder(BorderFactory
				.createBevelBorder(BevelBorder.LOWERED));
		
		bottomPanel.add(menuButton, BorderLayout.LINE_START);
		bottomPanel.add(new JPanel(), BorderLayout.CENTER);
		bottomPanel.add(stateCombo, BorderLayout.LINE_END);
	}
	
	private String showInputDialog(String message, String title) {
		
		return JOptionPane.showInputDialog(this, message, 
				title, JOptionPane.PLAIN_MESSAGE);
	}

	public void update(Message msg) {
		//System.out.println("update");
		
		stateCombo.setSelectedItem(connection.getUserState());
		setIconImage(((ImageIcon) PKjabToolkit
		          .getStatusIcons()
				  .get(connection.getUserState()))
				  .getImage());

		viewport.removeAll();
		
		Iterator it = connection.getRoster().getContacts().iterator();
		for (; it.hasNext();) {
			Contact contact = (Contact) it.next();
			if (!contactPanels.add(contact)) {
				viewport.add(new ContactPanel(contact, user));
			}
		}
		getContentPane().validate();
	}

	public static void main(String[] args) {
		
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				PKjabToolkit.setupUI();
				
				StartupDialog dialog = new StartupDialog(null, 
						APP_NAME + " - " + ENTER_USERDATA, 
						true);
				
				if (dialog.getUser() == null 
						|| dialog.getUser().equalsIgnoreCase(""))
					new PKjab("pkjab", "jabber.ccc.de", "pkjab");
				else
					new PKjab(dialog.getUser(), dialog.getDomain(), dialog.getPassword());
			}
		});
	}
}
