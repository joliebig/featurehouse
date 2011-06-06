

package org.gjt.sp.jedit.options;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;
import java.io.*;

import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.pluginmgr.*;
import org.gjt.sp.util.*;


public class PluginManagerOptionPane extends AbstractOptionPane
{
	
	public PluginManagerOptionPane()
	{
		super("plugin-manager");
	} 

	
	protected void _init()
	{
		setLayout(new BorderLayout());

		JLabel locationLabel = new JLabel(jEdit.getProperty(
			"options.plugin-manager.location"));

		mirrorLabel = new JLabel();
		updateMirrorLabel();

		if(jEdit.getSettingsDirectory() != null)
		{
			settingsDir = new JRadioButton(jEdit.getProperty(
				"options.plugin-manager.settings-dir"));
			settingsDir.setToolTipText(MiscUtilities.constructPath(
				jEdit.getSettingsDirectory(),"jars"));
		}
		JRadioButton appDir = new JRadioButton(jEdit.getProperty(
				"options.plugin-manager.app-dir"));
		appDir.setToolTipText(MiscUtilities.constructPath(
			jEdit.getJEditHome(),"jars"));

		miraList = new JList(miraModel = new MirrorModel());
		miraList.setSelectionModel(new SingleSelectionModel());

		
		add(BorderLayout.NORTH,mirrorLabel);
		add(BorderLayout.CENTER,new JScrollPane(miraList));

		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new BoxLayout(buttonPanel,BoxLayout.Y_AXIS));

		buttonPanel.add(Box.createVerticalStrut(6));

		
		updateMirrors = new JButton(jEdit.getProperty(
			"options.plugin-manager.updateMirrors"));
		updateMirrors.addActionListener(new ActionHandler());
		updateMirrors.setEnabled(false);
		VFSManager.runInWorkThread(new UpdateMirrorsThread(false));
		JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		panel.add(updateMirrors);
		panel.add(updateStatus);
		panel.setAlignmentX(Component.LEFT_ALIGNMENT);
		buttonPanel.add(panel);

		buttonPanel.add(Box.createVerticalStrut(6));

		
		downloadSource = new JCheckBox(jEdit.getProperty(
			"options.plugin-manager.downloadSource"));
		downloadSource.setSelected(jEdit.getBooleanProperty("plugin-manager.downloadSource"));
		downloadSource.setAlignmentX(Component.LEFT_ALIGNMENT);
		buttonPanel.add(downloadSource);

		buttonPanel.add(Box.createVerticalStrut(6));

		
		deleteDownloads = new JCheckBox(jEdit.getProperty(
			"options.plugin-manager.deleteDownloads"));
		deleteDownloads.setSelected(jEdit.getBooleanProperty("plugin-manager.deleteDownloads"));
		deleteDownloads.setAlignmentX(Component.LEFT_ALIGNMENT);
		buttonPanel.add(deleteDownloads);

		buttonPanel.add(Box.createVerticalStrut(6));

		
		ButtonGroup locGrp = new ButtonGroup();
		if(jEdit.getSettingsDirectory() != null)
			locGrp.add(settingsDir);
		locGrp.add(appDir);
		JPanel locPanel = new JPanel();
		locPanel.setBorder(new EmptyBorder(3,12,0,0));
		locPanel.setLayout(new BoxLayout(locPanel,BoxLayout.Y_AXIS));
		if(jEdit.getSettingsDirectory() != null)
		{
			locPanel.add(settingsDir);
			locPanel.add(Box.createVerticalStrut(3));
		}
		locPanel.add(appDir);
		locationLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		locPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		buttonPanel.add(locationLabel);
		buttonPanel.add(locPanel);

		buttonPanel.add(Box.createGlue());
		add(BorderLayout.SOUTH,buttonPanel);

		if (jEdit.getBooleanProperty("plugin-manager.installUser")
			&& jEdit.getSettingsDirectory() != null)
			settingsDir.setSelected(true);
		else
			appDir.setSelected(true);
	} 

	
	protected void _save()
	{
		jEdit.setBooleanProperty("plugin-manager.installUser",
			settingsDir != null && settingsDir.isSelected());
		jEdit.setBooleanProperty("plugin-manager.downloadSource",downloadSource.isSelected());
		jEdit.setBooleanProperty("plugin-manager.deleteDownloads",deleteDownloads.isSelected());

		if(miraList.getSelectedIndex() != -1)
		{
			String currentMirror = miraModel.getID(miraList.getSelectedIndex());
			String previousMirror = jEdit.getProperty("plugin-manager.mirror.id");

			if (!previousMirror.equals(currentMirror))
			{
				jEdit.setProperty("plugin-manager.mirror.id",currentMirror);
				jEdit.setProperty("plugin-manager.mirror.name",(String) miraModel.getElementAt(miraList.getSelectedIndex()));
				updateMirrorLabel();
				
			}
		}
	} 

	

	
	private JLabel mirrorLabel;

	private JRadioButton settingsDir;
	private JCheckBox downloadSource;
	private JCheckBox deleteDownloads;

	private MirrorModel miraModel;
	private JList miraList;
	
	private JButton updateMirrors;
	
	private final JLabel updateStatus = new JLabel();
	

	
	private void updateMirrorLabel()
	{
		String currentMirror = jEdit.getProperty("plugin-manager.mirror.id");
		String mirrorName;
		if (currentMirror.equals(MirrorList.Mirror.NONE))
		{
			mirrorName = "Plugin Central default";
		}
		else
		{
			mirrorName = jEdit.getProperty("plugin-manager.mirror.name");
			if (mirrorName == null) mirrorName = currentMirror;
		}
		mirrorLabel.setText(jEdit.getProperty(
			"options.plugin-manager.mirror") + ' ' + mirrorName);
	} 

	

	
	static class MirrorModel extends AbstractListModel
	{
		private List<MirrorList.Mirror> mirrors;

		MirrorModel()
		{
			mirrors = new ArrayList<MirrorList.Mirror>();
		}

		public String getID(int index)
		{
			return mirrors.get(index).id;
		}

		public int getSize()
		{
			return mirrors.size();
		}

		public Object getElementAt(int index)
		{
			MirrorList.Mirror mirror = mirrors.get(index);
			if(mirror.id.equals(MirrorList.Mirror.NONE))
				return jEdit.getProperty("options.plugin-manager.none");
			else
				return mirror.continent+": "+mirror.description+" ("+mirror.location+')';
		}

		public void setList(List<MirrorList.Mirror> mirrors)
		{
			this.mirrors = mirrors;
			fireContentsChanged(this,0,mirrors.size() - 1);
		}
	} 

	
	static class SingleSelectionModel extends DefaultListSelectionModel
	{
		SingleSelectionModel()
		{
			setSelectionMode(SINGLE_SELECTION);
		}

		public void removeSelectionInterval(int index0, int index1) {}
	} 

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			updateMirrors.setEnabled(false);
			updateStatus.setText(jEdit.getProperty("options.plugin-manager.workthread"));
			VFSManager.runInWorkThread(new UpdateMirrorsThread(true));
		}
	} 

	
	
	class UpdateMirrorsThread extends WorkRequest
	{
		private boolean download;

		UpdateMirrorsThread(boolean download)
		{
			this.download = download;
		}

		
		public void run()
		{
			try
			{
				setStatus(jEdit.getProperty("options.plugin-manager.workthread"));
				setMaximum(3);
				setValue(0);

				final List<MirrorList.Mirror> mirrors = new ArrayList<MirrorList.Mirror>();
				try
				{
					MirrorList mirrorList = new MirrorList(download, this);
					if (download)
						saveMirrorList(mirrorList.xml);

					mirrors.addAll(mirrorList.mirrors);
				}
				catch (Exception ex)
				{
					if (download)
					{
						Log.log(Log.ERROR,this,ex);
						GUIUtilities.error(PluginManagerOptionPane.this,
								"ioerror",new String[] { ex.toString() });
					}
				}

				SwingUtilities.invokeLater(new Runnable()
				{
					public void run()
					{
						miraModel.setList(mirrors);

						String id = jEdit.getProperty("plugin-manager.mirror.id");
						int size = miraModel.getSize();
						for (int i=0; i < size; i++)
						{
							if (size == 1 || miraModel.getID(i).equals(id))
							{
								miraList.setSelectedIndex(i);
								break;
							}
						}
					}
				});

				setValue(3);
			}
			finally
			{
				updateMirrors.setEnabled(true);
				updateStatus.setText(null);
			}
		} 

		
		private void saveMirrorList(String xml)
		{
			String settingsDirectory = jEdit.getSettingsDirectory();
			if(settingsDirectory == null)
				return;

			File mirrorList = new File(MiscUtilities.constructPath(
				settingsDirectory,"mirrorList.xml"));
			OutputStream out = null;

			try
			{
				out = new BufferedOutputStream(new FileOutputStream(mirrorList));
				IOUtilities.copyStream(null, new ByteArrayInputStream(xml.getBytes()), out, false);
			}
			catch (IOException e)
			{
				Log.log(Log.ERROR,this, "Unable to write cached mirror list : " + mirrorList);
			}
			finally
			{
				IOUtilities.closeQuietly(out);
			}
		} 
	} 
}
