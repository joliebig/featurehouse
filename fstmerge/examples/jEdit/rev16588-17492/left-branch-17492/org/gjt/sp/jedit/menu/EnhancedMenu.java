

package org.gjt.sp.jedit.menu;


import javax.swing.event.*;
import javax.swing.*;
import java.util.StringTokenizer;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.*;


public class EnhancedMenu extends JMenu implements MenuListener
{
	
	public EnhancedMenu(String name)
	{
		this(name,jEdit.getProperty(name.concat(".label")),
			jEdit.getActionContext());
	} 

	
	public EnhancedMenu(String name, String label)
	{
		this(name,label,jEdit.getActionContext());
	} 

	
	public EnhancedMenu(String name, String label, ActionContext context)
	{
		this.context = context;
		if(label == null)
			label = name;

		char mnemonic;
		int index = label.indexOf('$');
		if(index != -1 && label.length() - index > 1)
		{
			mnemonic = Character.toLowerCase(label.charAt(index + 1));
			label = label.substring(0,index).concat(label.substring(++index));
		}
		else
			mnemonic = '\0';

		setText(label);
		if(!OperatingSystem.isMacOS())
			setMnemonic(mnemonic);

		String menuItems = jEdit.getProperty(name);
		if(menuItems != null)
		{
			StringTokenizer st = new StringTokenizer(menuItems);
			while(st.hasMoreTokens())
			{
				String menuItemName = st.nextToken();
				if(menuItemName.equals("-"))
					addSeparator();
				else
					add(GUIUtilities.loadMenuItem(context,menuItemName,true));
			}
		}

		initialComponentCount = getMenuComponentCount();

		providerCode = jEdit.getProperty(name + ".code");

		ebStub = new EditBusStub(name);
		ebStub.menuOutOfDate = true;

		addMenuListener(this);

		if(providerCode != null)
			EditBus.addToBus(ebStub);
	} 

	
	public void menuSelected(MenuEvent evt)
	{
		init();
	} 

	public void menuDeselected(MenuEvent e) {}

	public void menuCanceled(MenuEvent e) {}

	
	public void init()
	{
		if(providerCode == null)
			return;

		if(provider == null)
		{
			Object obj = BeanShell.eval(null,
				BeanShell.getNameSpace(),
				providerCode);
			provider = (DynamicMenuProvider)obj;
		}

		if(provider == null)
		{
			
			providerCode = null;
			return;
		}

		if(ebStub.menuOutOfDate || provider.updateEveryTime())
		{
			ebStub.menuOutOfDate = false;

			while(getMenuComponentCount() != initialComponentCount)
				remove(getMenuComponentCount() - 1);

			if(provider != null)
				provider.update(this);
		}
	} 

	
	protected int initialComponentCount;
	protected ActionContext context;

	protected String providerCode;
	protected DynamicMenuProvider provider;

	protected EditBusStub ebStub;

	
	protected void finalize() throws Exception
	{
		if(ebStub != null)
			EditBus.removeFromBus(ebStub);
	} 

	

	
	
	static class EditBusStub implements EBComponent
	{
		String name;
		boolean menuOutOfDate;

		EditBusStub(String name)
		{
			this.name = name;
			menuOutOfDate = true;
		}

		public void handleMessage(EBMessage msg)
		{
			if(msg instanceof DynamicMenuChanged
				&& name.equals(((DynamicMenuChanged)msg)
				.getMenuName()))
			{
				menuOutOfDate = true;
			}
			else if(msg instanceof PropertiesChanged)
			{
				
				
				menuOutOfDate = true;
			}
		}
	} 
}
