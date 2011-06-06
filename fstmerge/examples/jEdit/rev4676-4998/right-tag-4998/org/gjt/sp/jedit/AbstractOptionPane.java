

package org.gjt.sp.jedit;


import javax.swing.border.EmptyBorder;
import javax.swing.*;
import java.awt.*;






public class AbstractOptionPane extends JPanel implements OptionPane
{
	
	
	public AbstractOptionPane(String name)
	{
		this.name = name;
		setLayout(gridBag = new GridBagLayout());
	} 

	
	
	public String getName()
	{
		return name;
	} 

	
	
	public Component getComponent()
	{
		return this;
	} 

	
	
	
	public void init()
	{
		if(!initialized)
		{
			initialized = true;
			_init();
		}
	} 

	
	
	
	public void save()
	{
		if(initialized)
			_save();
	} 

	
	
	public void addComponent(String label, Component comp)
	{
		JLabel l = new JLabel(label);
		l.setBorder(new EmptyBorder(0,0,0,12));
		addComponent(l,comp,GridBagConstraints.BOTH);
	} 

	
	
	public void addComponent(String label, Component comp, int fill)
	{
		JLabel l = new JLabel(label);
		l.setBorder(new EmptyBorder(0,0,0,12));
		addComponent(l,comp,fill);
	} 

	
	
	public void addComponent(Component comp1, Component comp2)
	{
		addComponent(comp1,comp2,GridBagConstraints.BOTH);
	} 

	
	
	public void addComponent(Component comp1, Component comp2, int fill)
	{
		GridBagConstraints cons = new GridBagConstraints();
		cons.gridy = y++;
		cons.gridheight = 1;
		cons.gridwidth = 1;
		cons.weightx = 0.0f;
		cons.insets = new Insets(1,0,1,0);
		cons.fill = GridBagConstraints.BOTH;

		gridBag.setConstraints(comp1,cons);
		add(comp1);

		cons.fill = fill;
		cons.gridx = 1;
		cons.weightx = 1.0f;
		gridBag.setConstraints(comp2,cons);
		add(comp2);
	} 

	
	
	public void addComponent(Component comp)
	{
		GridBagConstraints cons = new GridBagConstraints();
		cons.gridy = y++;
		cons.gridheight = 1;
		cons.gridwidth = cons.REMAINDER;
		cons.fill = GridBagConstraints.NONE;
		cons.anchor = GridBagConstraints.WEST;
		cons.weightx = 1.0f;
		cons.insets = new Insets(1,0,1,0);

		gridBag.setConstraints(comp,cons);
		add(comp);
	} 

	
	
	public void addSeparator()
	{
		addComponent(Box.createVerticalStrut(6));

		JSeparator sep = new JSeparator(JSeparator.HORIZONTAL);

		GridBagConstraints cons = new GridBagConstraints();
		cons.gridy = y++;
		cons.gridheight = 1;
		cons.gridwidth = cons.REMAINDER;
		cons.fill = GridBagConstraints.BOTH;
		cons.anchor = GridBagConstraints.WEST;
		cons.weightx = 1.0f;
		

		gridBag.setConstraints(sep,cons);
		add(sep);

		addComponent(Box.createVerticalStrut(6));
	} 

	
	
	public void addSeparator(String label)
	{
		if(y != 0)
			addComponent(Box.createVerticalStrut(6));

		Box box = new Box(BoxLayout.X_AXIS);
		Box box2 = new Box(BoxLayout.Y_AXIS);
		box2.add(Box.createGlue());
		box2.add(new JSeparator(JSeparator.HORIZONTAL));
		box2.add(Box.createGlue());
		box.add(box2);
		JLabel l = new JLabel(jEdit.getProperty(label));
		l.setMaximumSize(l.getPreferredSize());
		box.add(l);
		Box box3 = new Box(BoxLayout.Y_AXIS);
		box3.add(Box.createGlue());
		box3.add(new JSeparator(JSeparator.HORIZONTAL));
		box3.add(Box.createGlue());
		box.add(box3);

		GridBagConstraints cons = new GridBagConstraints();
		cons.gridy = y++;
		cons.gridheight = 1;
		cons.gridwidth = cons.REMAINDER;
		cons.fill = GridBagConstraints.BOTH;
		cons.anchor = GridBagConstraints.WEST;
		cons.weightx = 1.0f;
		cons.insets = new Insets(1,0,1,0);

		gridBag.setConstraints(box,cons);
		add(box);
	} 

	
	
	protected boolean initialized;

	
	protected GridBagLayout gridBag;

	
	protected int y;

	
	protected void _init() {}

	
	protected void _save() {}
	

	
	private String name;
	
}
