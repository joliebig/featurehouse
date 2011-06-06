

package org.gjt.sp.jedit.gui.statusbar;


import org.gjt.sp.jedit.View;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.gui.EnhancedDialog;
import org.gjt.sp.util.Log;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.PrintStream;
import java.io.ByteArrayOutputStream;



public class ErrorsWidgetFactory implements StatusWidgetFactory
{
	
	public Widget getWidget(View view)
	{
		Widget errorWidget = new ErrorWidget(view);
		return errorWidget;
	} 

	
	private static class ErrorWidget implements Widget
	{
		private final ErrorHighlight errorHighlight;

		ErrorWidget(View view)
		{
			errorHighlight = new ErrorHighlight(view);
		}

		public JComponent getComponent()
		{
			return errorHighlight;
		}

		public void update()
		{
			errorHighlight.update();
		}

		public void propertiesChanged()
		{
		}
	} 

	
	private static class ErrorHighlight extends JLabel implements ActionListener
	{
		private int currentSize;

		
		ErrorHighlight(View view)
		{
			setForeground(jEdit.getColorProperty("view.status.foreground"));
			setBackground(jEdit.getColorProperty("view.status.background"));
			addMouseListener(new MyMouseAdapter(view));
		} 

		
		@Override
		public void addNotify()
		{
			super.addNotify();
			update();
			int millisecondsPerMinute = 1000;

			timer = new Timer(millisecondsPerMinute, this);
			timer.start();
			ToolTipManager.sharedInstance().registerComponent(this);
		} 


		
		@Override
		public void removeNotify()
		{
			timer.stop();
			ToolTipManager.sharedInstance().unregisterComponent(this);
			super.removeNotify();
		} 

		
		@Override
		public Point getToolTipLocation(MouseEvent event)
		{
			return new Point(event.getX(), -20);
		} 

		
		public void actionPerformed(ActionEvent e)
		{
			update();
		} 

		private Timer timer;

		
		private void update()
		{
			int size = Log.throwables.size();
			if (size != currentSize)
			{
				currentSize = size;
				if (size == 0)
				{
					setForeground(jEdit.getColorProperty("view.status.foreground"));
					setText(null);
					setToolTipText(size + " error");
				}
				else
				{
					setForeground(Color.red);
					setText(Integer.toString(size) + " error(s)");
					setToolTipText(size + " error(s)");
				}
			}
		} 

		
		private class MyMouseAdapter extends MouseAdapter
		{
			private final View view;

			MyMouseAdapter(View view)
			{
				this.view = view;
			}

			@Override
				public void mouseClicked(MouseEvent e)
			{
				if (Log.throwables.isEmpty())
					return;
				if (GUIUtilities.isRightButton(e.getModifiers()))
				{
					JPopupMenu menu = GUIUtilities.loadPopupMenu("errorwidget.popupmenu");
					GUIUtilities.showPopupMenu(menu, ErrorHighlight.this, e.getX(), e.getY());

				}
				else if (e.getClickCount() == 2)
					new ErrorDialog(view);

			}
		} 

	} 

	
	private static class ErrorDialog extends EnhancedDialog
	{
		private final JTextArea textArea;
		private final ByteArrayOutputStream byteArrayOutputStream;
		private final PrintStream printStream;
		private final JButton removeThisError;
		private final JButton removeAllErrors;
		private final Object[] throwables;
		private final JComboBox combo;

		
		private ErrorDialog(Frame view)
		{
			super(view, "Errors", false);
			byteArrayOutputStream = new ByteArrayOutputStream();
			printStream = new PrintStream(byteArrayOutputStream);
			throwables = Log.throwables.toArray();
			textArea = new JTextArea();
			textArea.setEditable(false);
			if (throwables.length != 0)
			{
				Throwable throwable = (Throwable) throwables[0];
				setThrowable(throwable);
			}
			combo = new JComboBox(throwables);
			combo.addItemListener(new ItemListener()
			{
				public void itemStateChanged(ItemEvent e)
				{
					setThrowable((Throwable) combo.getSelectedItem());
				}
			});
			getContentPane().add(combo, BorderLayout.NORTH);
			getContentPane().add(new JScrollPane(textArea));



			Box buttons = new Box(BoxLayout.X_AXIS);
			buttons.add(Box.createGlue());

			buttons.add(removeThisError = new JButton(jEdit.getProperty("grab-key.remove")));
			buttons.add(Box.createHorizontalStrut(6));
			buttons.add(removeAllErrors = new JButton(jEdit.getProperty("common.clearAll")));

			ErrorDialog.MyActionListener actionListener = new MyActionListener();
			removeThisError.addActionListener(actionListener);
			removeAllErrors.addActionListener(actionListener);
			buttons.add(Box.createGlue());


			getContentPane().add(buttons, BorderLayout.SOUTH);
			pack();
			GUIUtilities.loadGeometry(this,"status.errorWidget");
			setVisible(true);
		} 

		
		private void setThrowable(Throwable throwable)
		{
			if (throwable == null)
			{
				textArea.setText(null);
			}
			else
			{
				throwable.printStackTrace(printStream);
				textArea.setText(byteArrayOutputStream.toString());
				textArea.setCaretPosition(0);
				byteArrayOutputStream.reset();
			}
		} 

		
		@Override
		public void dispose()
		{
			GUIUtilities.saveGeometry(this, "status.errorWidget");
			super.dispose();
		} 

		
		@Override
		public void ok()
		{
			dispose();
		} 

		
		@Override
		public void cancel()
		{
			dispose();
		} 

		
		private class MyActionListener implements ActionListener
		{
			public void actionPerformed(ActionEvent e)
			{
				Object source = e.getSource();
				if (source == removeThisError)
				{
					Throwable throwable = (Throwable) combo.getSelectedItem();
					if (throwable != null)
					{
						Log.throwables.remove(throwable);
						combo.removeItem(throwable);
						if (combo.getItemCount() == 0)
						{
							dispose();
						}
					}
				}
				else if (source == removeAllErrors)
				{
					for (Object throwable : throwables)
					{
						Log.throwables.remove(throwable);
					}
					dispose();
				}
			}
		} 
	} 
}
