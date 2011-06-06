

package installer;

import javax.swing.border.*;
import javax.swing.*;
import javax.swing.text.JTextComponent;
import java.awt.event.*;
import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.util.*;


public class SwingInstall extends JFrame
{
	public SwingInstall()
	{
		installer = new Install();
		osTasks = OperatingSystem.getOperatingSystem().getOSTasks(installer);

		appName = installer.getProperty("app.name");
		appVersion = installer.getProperty("app.version");

		setTitle(appName + " " + appVersion + " installer");

		JPanel content = new JPanel(new WizardLayout());
		setContentPane(content);

		caption = new JLabel();
		caption.setFont(new Font("SansSerif",Font.BOLD,18));

		ActionHandler actionHandler = new ActionHandler();

		cancelButton = new JButton("Cancel");
		cancelButton.setRequestFocusEnabled(false);
		cancelButton.addActionListener(actionHandler);
		prevButton = new JButton("Previous");
		prevButton.setRequestFocusEnabled(false);
		prevButton.addActionListener(actionHandler);
		nextButton = new JButton();
		nextButton.setRequestFocusEnabled(false);
		nextButton.addActionListener(actionHandler);

		content.add(caption);
		content.add(cancelButton);
		content.add(prevButton);
		content.add(nextButton);

		String clazz = OperatingSystem.getOperatingSystem()
				.getClass().getName();
		String completedInfo = "done-" + clazz.substring(
			clazz.indexOf('$') + 1) + ".html";

		pages = new Component[] {
			new TextPanel(installer.getProperty("app.readme")),
			new TextPanel(installer.getProperty("app.license")),
			chooseDirectory = new ChooseDirectory(),
			selectComponents = new SelectComponents(),
			progress = new SwingProgress(),
			new TextPanel(completedInfo)
		};

		for(int i = 0; i < pages.length; i++)
			content.add(pages[i]);

		pageChanged();

		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowHandler());

		Dimension screen = getToolkit().getScreenSize();
		pack();
		setLocation((screen.width - getSize().width) / 2,
			(screen.height - getSize().height) / 2);
		setVisible(true);
	}

	
	
	Install installer;
	OperatingSystem.OSTask[] osTasks;
	String appName;
	String appVersion;

	JLabel caption;

	ChooseDirectory chooseDirectory;
	SelectComponents selectComponents;
	SwingProgress progress;

	JButton cancelButton;
	JButton prevButton;
	JButton nextButton;
	Component[] pages;
	int currentPage;

	private static final int PADDING = 12;

	void install()
	{
		Vector components = new Vector();
		int size = 0;

		JPanel comp = selectComponents.comp;
		Vector ids = selectComponents.filesets;

		for(int i = 0; i < comp.getComponentCount(); i++)
		{
			if(((JCheckBox)comp.getComponent(i))
				.getModel().isSelected())
			{
				size += installer.getIntegerProperty(
					"comp." + ids.elementAt(i) + ".real-size");
				components.addElement(installer.getProperty(
					"comp." + ids.elementAt(i) + ".fileset"));
			}
		}

		String installDir = chooseDirectory.installDir.getText();

		Map osTaskDirs = chooseDirectory.osTaskDirs;
		Iterator keys = osTaskDirs.keySet().iterator();
		while(keys.hasNext())
		{
			OperatingSystem.OSTask osTask = (OperatingSystem.OSTask)keys.next();
			String dir = ((JTextField)osTaskDirs.get(osTask)).getText();
			if(dir != null && dir.trim().length() != 0)
			{
				osTask.setEnabled(true);
				osTask.setDirectory(dir);
			}
			else
				osTask.setEnabled(false);
		}

		InstallThread thread = new InstallThread(
			installer,progress,
			installDir,osTasks,
			size,components);
		progress.setThread(thread);
		thread.start();
	}

	private void pageChanged()
	{
		switch(currentPage)
		{
		case 0:
			caption.setText("Installing " + appName);

			nextButton.setText("Next");
			prevButton.setEnabled(false);
			nextButton.setEnabled(true);
			break;
		case 1:
			caption.setText(installer.getProperty("app.license.title"));

			nextButton.setText("Next");
			prevButton.setEnabled(true);
			nextButton.setEnabled(true);
			break;
		case 2:
			caption.setText("Specify where " + appName
				+ " is to be installed");

			nextButton.setText("Next");
			prevButton.setEnabled(true);
			if(!chooseDirectory.isOK())nextButton.setEnabled(false);
			break;
		case 3:
			caption.setText("Choose components to install");

			nextButton.setText("Install");
			prevButton.setEnabled(true);
			nextButton.setEnabled(true);
			break;
		case 4:
			caption.setText("Installing " + appName);

			nextButton.setText("Finish");
			prevButton.setEnabled(false);
			nextButton.setEnabled(false);
			install();
			break;
		case 5:
			caption.setText("Installation complete");

			nextButton.setText("Finish");
			prevButton.setEnabled(false);
			nextButton.setEnabled(true);
			cancelButton.setEnabled(false);
			break;
		}

		getRootPane().invalidate();
		getRootPane().validate();
	}

	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			Object source = evt.getSource();
			if(source == cancelButton)
				System.exit(0);
			else if(source == prevButton)
			{
				currentPage--;
				pageChanged();
			}
			else if(source == nextButton)
			{
				if(currentPage == pages.length - 1)
					System.exit(0);
				else
				{
					currentPage++;
					pageChanged();
				}
			}
		}
	}

	class WindowHandler extends WindowAdapter
	{
		public void windowClosing(WindowEvent evt)
		{
			System.exit(0);
		}
	}

	class WizardLayout implements LayoutManager
	{
		public void addLayoutComponent(String name, Component comp)
		{
		}

		public void removeLayoutComponent(Component comp)
		{
		}

		public Dimension preferredLayoutSize(Container parent)
		{
			Dimension dim = new Dimension();

			Dimension captionSize = caption.getPreferredSize();
			dim.width = captionSize.width;

			for(int i = 0; i < pages.length; i++)
			{
				Dimension _dim = pages[i].getPreferredSize();
				dim.width = Math.max(_dim.width,dim.width);
				dim.height = Math.max(_dim.height,dim.height);
			}

			dim.width += PADDING * 2;
			dim.height += PADDING * 2;
			dim.height += nextButton.getPreferredSize().height;
			dim.height += captionSize.height;
			return dim;
		}

		public Dimension minimumLayoutSize(Container parent)
		{
			return preferredLayoutSize(parent);
		}

		public void layoutContainer(Container parent)
		{
			Dimension size = parent.getSize();

			Dimension captionSize = caption.getPreferredSize();
			caption.setBounds(PADDING,PADDING,captionSize.width,
				captionSize.height);

			
			Dimension buttonSize = cancelButton.getPreferredSize();
			buttonSize.width = Math.max(buttonSize.width,prevButton.getPreferredSize().width);
			buttonSize.width = Math.max(buttonSize.width,nextButton.getPreferredSize().width);

			
			cancelButton.setBounds(
				PADDING,
				size.height - buttonSize.height - PADDING,
				buttonSize.width,
				buttonSize.height);

			
			prevButton.setBounds(
				size.width - buttonSize.width * 2 - 6 - PADDING,
				size.height - buttonSize.height - PADDING,
				buttonSize.width,
				buttonSize.height);

			nextButton.setBounds(
				size.width - buttonSize.width - PADDING,
				size.height - buttonSize.height - PADDING,
				buttonSize.width,
				buttonSize.height);

			
			Rectangle currentPageBounds = new Rectangle();
			currentPageBounds.x = PADDING;
			currentPageBounds.y = PADDING * 2 + captionSize.height;
			currentPageBounds.width = size.width - currentPageBounds.x
				- PADDING;
			currentPageBounds.height = size.height - buttonSize.height
				- currentPageBounds.y - PADDING * 2;

			for(int i = 0; i < pages.length; i++)
			{
				Component page = pages[i];
				page.setBounds(currentPageBounds);
				page.setVisible(i == currentPage);
			}
		}
	}

	class TextPanel extends JPanel
	{
		TextPanel(String file)
		{
			super(new BorderLayout());

			JEditorPane text = new JEditorPane();

			try
			{
				text.setPage(TextPanel.this.getClass().getResource(file));
			}
			catch(Exception e)
			{
				text.setText("Error loading '" + file + "'");
				e.printStackTrace();
			}

			text.setEditable(false);

			JScrollPane scrollPane = new JScrollPane(text);
			Dimension dim = new Dimension();
			dim.width = 450;
			dim.height = 200;
			scrollPane.setPreferredSize(dim);
			TextPanel.this.add(BorderLayout.CENTER,scrollPane);
		}
	}

	class DirVerifier extends InputVerifier
	{
		private JTextComponent message;
		private Object pos;
		private JComponent parent;
		
		public DirVerifier(JComponent parent, Object pos)
		{
			super();
			message = new JTextArea(" ");
			message.setEditable(false);
			message.setBackground(parent.getBackground());
			this.parent = parent;
			this.pos = pos;
		}
		
		public boolean shouldYieldFocus(JComponent input)
		{
			return verify(input);
		}
		
		public boolean verify(JComponent input)
		{
			if(input instanceof JTextComponent)
			{
				String dir = ((JTextComponent)input).getText();
				if(checkNull(dir) &&
					checkExistNotDirectory(dir) &&
					checkExistNotEmpty(dir) &&
					checkRelative(dir))
				{
					
					if(message.getParent()!=null)
					{
						SwingUtilities.invokeLater(new Runnable()
							{
								public void run()
								{
									parent.remove(message);
									parent.revalidate();
									parent.repaint();
								}
							});
					}
				}
				else
				{
					if(message.getParent()==null)
					{
						SwingUtilities.invokeLater(new Runnable()
							{
								public void run()
								{
									parent.add(message,pos);
									parent.revalidate();
									parent.repaint();
								}
							});
					}
					else message.repaint();
				}
			}
			return true;
		}
		
		private boolean checkNull(String file)
		{
			if(file.trim().length()==0)
			{
				message.setForeground(Color.red);
				message.setText(installer.getProperty("dir.null"));
				return false;
			}
			else return true;
		}
		
		private boolean checkRelative(String dir)
		{
			File f = new File(dir);
			if(!f.isAbsolute())
			{
				String msg = installer.getProperty("dir.relative");
				try
				{
					String full = f.getCanonicalPath();
					message.setForeground(Color.orange);
					message.setText(msg+'\n'+full);
				}
				catch(IOException ioe)
				{
					message.setForeground(Color.red);
					msg = installer.getProperty("dir.cant-resolve");
					message.setText(msg);
				}
				return false;
			}
			else return true;
		}
		
		private boolean checkExistNotDirectory(String dir)
		{
			File f = new File(dir);
			if(f.exists() && !f.isDirectory())
			{
				message.setForeground(Color.red);
				message.setText(installer.getProperty("dir.not-directory"));
				return false;
			}
			else return true;
		}
		
		private boolean checkExistNotEmpty(String dir)
		{
			File f = new File(dir);
			String[]cnt = f.list();
			if(cnt!=null && cnt.length>0)
			{
				message.setForeground(Color.orange);
				message.setText(installer.getProperty("dir.not-empty"));
				return false;
			}
			else return true;
		}
	}
	
	class ChooseDirectory extends JPanel
	{
		JTextField installDir;
		Map osTaskDirs;

		ChooseDirectory()
		{
			super(new BorderLayout());
			osTaskDirs = new HashMap();

			
			JPanel directoryPanel = new JPanel(new GridBagLayout());

			installDir = addField(directoryPanel,"Install program in:",
				OperatingSystem.getOperatingSystem()
				.getInstallDirectory(appName,appVersion));

			installDir.addFocusListener(new FocusAdapter()
				{
					public void focusLost(FocusEvent fe)
					{
						nextButton.setEnabled(isOK());
					}
				});
			for(int i = 0; i < osTasks.length; i++)
			{
				OperatingSystem.OSTask osTask = osTasks[i];
				String label = osTask.getLabel();
				if(label != null)
				{
					JTextField field = addField(directoryPanel,label,
						osTask.getDirectory());
					osTaskDirs.put(osTask,field);
				}
			}
			ChooseDirectory.this.add(BorderLayout.NORTH,directoryPanel);
		}

		boolean isOK()
		{
			if(installDir.getText().length()==0)return false;
			File f = new File(installDir.getText());
			return !(f.exists()&&!f.isDirectory());
		}

		private GridBagConstraints c = new GridBagConstraints();
		private JTextField addField(JPanel directoryPanel, String label,
			String defaultText)
		{
			
			
			c.gridy++;
			
			
			JTextField field = new JTextField(defaultText);
			c.insets.bottom=3;
			c.gridx=0;
			c.gridwidth=3;
			c.insets.left=0;
			c.insets.right=0;
			c.anchor=GridBagConstraints.LINE_START;
			DirVerifier verif = new DirVerifier(directoryPanel,c.clone());
			
			
			field.setInputVerifier(verif);
			
			c.insets.bottom=12;
			c.gridx=0;
			c.gridy++;
			c.gridwidth=1;
			c.anchor=GridBagConstraints.LINE_END;
			directoryPanel.add(new JLabel(label,SwingConstants.RIGHT),c);

			c.gridx=1;
			c.fill=GridBagConstraints.HORIZONTAL;
			c.anchor=GridBagConstraints.CENTER;
			c.insets.left=12;
			c.insets.right=12;
			c.weightx=1.0;
			directoryPanel.add(field,c);
			
			
			JButton choose = new JButton("Choose...");
			choose.setRequestFocusEnabled(false);
			choose.addActionListener(new ActionHandler(field));
			c.gridx=2;
			c.insets.left=0;
			c.insets.right=0;
			c.fill=GridBagConstraints.NONE;
			c.weightx=0;
			directoryPanel.add(choose,c);

			return field;
		}

		class ActionHandler implements ActionListener
		{
			JTextField field;

			ActionHandler(JTextField field)
			{
				this.field = field;
			}

			public void actionPerformed(ActionEvent evt)
			{
				File directory = new File(field.getText());
				JFileChooser chooser = new JFileChooser(directory.getParent());
				chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				chooser.setSelectedFile(directory);

				if(chooser.showOpenDialog(SwingInstall.this)
					== JFileChooser.APPROVE_OPTION)
				{
						field.setText(chooser.getSelectedFile().getPath());
						field.getInputVerifier().verify(field);
				}
			}
		}
	}

	class SelectComponents extends JPanel
	implements ActionListener
	{
		JPanel comp;
		JLabel sizeLabel;
		Vector filesets;

		SelectComponents()
		{
			super(new BorderLayout());

			comp = createCompPanel();
			SelectComponents.this.add(BorderLayout.NORTH,comp);

			sizeLabel = new JLabel("",SwingConstants.LEFT);
			SelectComponents.this.add(BorderLayout.SOUTH,sizeLabel);

			updateSize();
		}

		public void actionPerformed(ActionEvent evt)
		{
			updateSize();
		}

		private JPanel createCompPanel()
		{
			filesets = new Vector();

			int count = installer.getIntegerProperty("comp.count");
			JPanel panel = new JPanel(new GridLayout(count,1));

			String osClass = OperatingSystem.getOperatingSystem()
				.getClass().getName();
			osClass = osClass.substring(osClass.indexOf('$') + 1);

			for(int i = 0; i < count; i++)
			{
				String os = installer.getProperty("comp." + i + ".os");

				if(os != null && !osClass.equals(os))
					continue;

				JCheckBox checkBox = new JCheckBox(
					installer.getProperty("comp." + i + ".name")
					+ " (" + installer.getProperty("comp." + i
					+ ".disk-size") + "Kb)");
				checkBox.getModel().setSelected(true);
				checkBox.addActionListener(this);
				checkBox.setRequestFocusEnabled(false);

				filesets.addElement(new Integer(i));

				panel.add(checkBox);
			}

			Dimension dim = panel.getPreferredSize();
			dim.width = Integer.MAX_VALUE;
			panel.setMaximumSize(dim);

			return panel;
		}

		private void updateSize()
		{
			int size = 0;

			for(int i = 0; i < filesets.size(); i++)
			{
				if(((JCheckBox)comp.getComponent(i))
					.getModel().isSelected())
				{
					size += installer.getIntegerProperty("comp."
						+ filesets.elementAt(i)
						+ ".disk-size");
				}
			}

			sizeLabel.setText("Estimated disk usage of selected"
				+ " components: " + size + "Kb");
		}
	}

	class SwingProgress extends JPanel implements Progress
	{
		JProgressBar progress;
		InstallThread thread;

		SwingProgress()
		{
			super(new BorderLayout());

			progress = new JProgressBar();
			progress.setStringPainted(true);

			SwingProgress.this.add(BorderLayout.NORTH,progress);
		}

		public void setMaximum(final int max)
		{
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					progress.setMaximum(max);
				}
			});
		}

		public void advance(final int value)
		{
			try
			{
				SwingUtilities.invokeAndWait(new Runnable()
				{
					public void run()
					{
						progress.setValue(progress
							.getValue() + value);
					}
				});
				Thread.yield();
			}
			catch(Exception e)
			{
			}
		}

		public void done()
		{
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					currentPage++;
					pageChanged();
				}
			});
		}

		public void error(final String message)
		{
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					dispose();
					JOptionPane.showMessageDialog(null,
						message,
						"Installation aborted",
						JOptionPane.ERROR_MESSAGE);
					System.exit(1);
				}
			});
		}

		public void message(final String message)
		{
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					progress.setString(message);
				}
			});
		}

		public void setThread(InstallThread thread)
		{
			this.thread = thread;
		}
	}
}
