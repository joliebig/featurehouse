package org.gjt.sp.jedit.gui;

import java.util.HashMap;
import java.util.Map;

import javax.swing.JOptionPane;

import org.gjt.sp.jedit.ActionSet;
import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.EBComponent;
import org.gjt.sp.jedit.EBMessage;
import org.gjt.sp.jedit.EditAction;
import org.gjt.sp.jedit.EditBus;
import org.gjt.sp.jedit.Mode;
import org.gjt.sp.jedit.View;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.gui.DockableWindowManager.DockingLayout;
import org.gjt.sp.jedit.msg.BufferUpdate;
import org.gjt.sp.jedit.msg.EditPaneUpdate;
import org.gjt.sp.jedit.msg.ViewUpdate;
import org.gjt.sp.jedit.options.DockingOptionPane;


public class DockingLayoutManager implements EBComponent
{

	private static final String NO_SETTINGS_MESSAGE = "no-settings.message";
	private static final String SAVE_LAYOUT_FAILED = "save-layout-failed.message";
	private static final String LOAD_LAYOUT_TITLE = "load-layout.title";
	private static final String LOAD_LAYOUT_MESSAGE = "load-layout.message";
	private static final String SAVE_LAYOUT_TITLE = "save-layout.title";
	private static final String SAVE_LAYOUT_MESSAGE = "save-layout.message";
	private static ActionSet actions;
	private static DockingLayoutManager instance;
	private Map<View, String> currentMode;
	
	private DockingLayoutManager()
	{
		currentMode = new HashMap<View, String>();
	}
	private static boolean save(View view, String layoutName)
	{
		DockingLayout docking = view.getViewConfig().docking; 
		if (docking != null)
		{
			boolean ret = docking.saveLayout(layoutName, DockingLayout.NO_VIEW_INDEX);
			if (! ret)
				return false;
			addAction(layoutName);
		}
		return true;
	}
	
	public static void saveAs(View view)
	{
		if (jEdit.getSettingsDirectory() == null)
		{
			JOptionPane.showMessageDialog(view, jEdit.getProperty(NO_SETTINGS_MESSAGE));
			return;
		}
		String layoutName = JOptionPane.showInputDialog(view,
			jEdit.getProperty(SAVE_LAYOUT_MESSAGE),
			jEdit.getProperty(SAVE_LAYOUT_TITLE),
			JOptionPane.QUESTION_MESSAGE);
		if (layoutName == null)
			return;
		if (! save(view, layoutName))
			JOptionPane.showMessageDialog(view, jEdit.getProperty(SAVE_LAYOUT_FAILED));
	}
	
	private static void load(View view, String layoutName)
	{
		DockingLayout docking = View.getDockingFrameworkProvider().createDockingLayout();
		if (docking.loadLayout(layoutName, DockingLayout.NO_VIEW_INDEX))
			view.getDockableWindowManager().setDockingLayout(docking);
	}
	
	public static void load(View view)
	{
		if (jEdit.getSettingsDirectory() == null)
		{
			JOptionPane.showMessageDialog(view, jEdit.getProperty(NO_SETTINGS_MESSAGE));
			return;
		}
		String layoutName = (String) JOptionPane.showInputDialog(view,
			jEdit.getProperty(LOAD_LAYOUT_MESSAGE),
			jEdit.getProperty(LOAD_LAYOUT_TITLE),
			JOptionPane.QUESTION_MESSAGE,
			null,
			getSavedLayouts(),
			null);
		if (layoutName == null)
			return;
		load(view, layoutName);
	}

	private static String[] getSavedLayouts()
	{
		DockingLayout docking = View.getDockingFrameworkProvider().createDockingLayout();
		String[] layouts = null;
		if (docking != null)
			layouts = docking.getSavedLayouts();
		if (layouts == null)
			return new String[0];
		return layouts;
	}
	
	private static void addAction(String layoutName)
	{
		if ((actions != null) && (! actions.contains(layoutName)))
			actions.addAction(new LoadPerspectiveAction(layoutName));
	}
	
	public static void init()
	{
		createActions();
		instance = new DockingLayoutManager();
		EditBus.addToBus(instance);
	}
	
	private static void createActions()
	{
		actions = new ActionSet("Docking Layouts");
		String[] layouts = getSavedLayouts();
		for (String layout: layouts)
			addAction(layout);
		jEdit.addActionSet(actions);
		actions.initKeyBindings();
	}
	
	public static void removeActions()
	{
		jEdit.removeActionSet(actions);
	}

	private static class LoadPerspectiveAction extends EditAction
	{
		private static final String LOAD_PREFIX = "load-";

		public LoadPerspectiveAction(String layoutName)
		{
			super(LOAD_PREFIX + layoutName, new String[] { layoutName });
			jEdit.setTemporaryProperty(LOAD_PREFIX + layoutName + ".label", LOAD_PREFIX + layoutName);
		}
		
		@Override
		public void invoke(View view)
		{
			DockingLayoutManager.load(view, (String) args[0]);
		}
	}

	private boolean canChangeEditMode(EBMessage message)
	{
		if (message instanceof BufferUpdate)
		{
			BufferUpdate bu = (BufferUpdate) message;
			Object what = bu.getWhat();
			if ((what == BufferUpdate.CLOSED) ||
				(what == BufferUpdate.CREATED) ||
				(what == BufferUpdate.PROPERTIES_CHANGED))
			{
				return true;
			}
		}
		else if (message instanceof EditPaneUpdate)
		{
			EditPaneUpdate ep = (EditPaneUpdate) message;
			Object what = ep.getWhat();
			if ((what == EditPaneUpdate.BUFFER_CHANGED) ||
				(what == EditPaneUpdate.CREATED))
			{
				return true;
			}
		}
		return false;
	}

	public void handleMessage(EBMessage message)
	{
		boolean autoLoadModeLayout = jEdit.getBooleanProperty(
			DockingOptionPane.AUTO_LOAD_MODE_LAYOUT_PROP, false);
		if (! autoLoadModeLayout)
			return;
		if (message instanceof ViewUpdate)
		{
			ViewUpdate vu = (ViewUpdate) message;
			if (vu.getWhat() == ViewUpdate.CLOSED)
			{
				View view = jEdit.getActiveView();
				String mode = currentMode.get(view);
				saveModeLayout(view, mode);
				return;
			}
		}
		
		View view = jEdit.getActiveView();
		if (view == null)
			return;
		if (! canChangeEditMode(message))
			return;
		String newMode = getCurrentEditMode(view);
		String mode = currentMode.get(view);
		boolean sameMode =
			(mode == null && newMode == null) ||
			(mode != null && mode.equals(newMode));
		if (! sameMode)
		{
			boolean autoSaveModeLayout = jEdit.getBooleanProperty(
				DockingOptionPane.AUTO_SAVE_MODE_LAYOUT_PROP, false);
			if (autoSaveModeLayout)
				saveModeLayout(view, mode);
			currentMode.put(view, newMode);
			loadModeLayout(view, newMode);
		}
	}

	private String getCurrentEditMode(View view)
	{
		Buffer buffer = view.getBuffer();
		if (buffer == null)
			return null;
		Mode bufferMode = buffer.getMode();
		if (bufferMode == null)
			return null;
		return bufferMode.getName();
	}

	private static final String GLOBAL_MODE = "DEFAULT";
	
	private void saveModeLayout(View view, String mode)
	{
		String modeLayout = getModePerspective(mode);
		if (modeLayout == null)
			return;
		save(view, modeLayout);
	}
	
	private void loadModeLayout(View view, String mode)
	{
		String modeLayout = getModePerspective(mode);
		if (modeLayout == null)
			return;
		load(view, modeLayout);
	}

	public static void loadCurrentModeLayout(View view)
	{
		if (view == null)
			return;
		String mode = instance.getCurrentEditMode(view);
		instance.loadModeLayout(view, mode);
	}
	
	public static void saveCurrentModeLayout(View view)
	{
		if (view == null)
			return;
		String mode = instance.getCurrentEditMode(view);
		instance.saveModeLayout(view, mode);
	}
	
	private String getModePerspective(String mode)
	{
		if (mode == null)
			mode = GLOBAL_MODE;
		return "mode-" + mode;
	}
}
