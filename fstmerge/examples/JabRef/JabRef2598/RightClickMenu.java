
package net.sf.jabref; 

import java.awt.Font; 
import java.awt.event.ActionEvent; 
import java.awt.event.ActionListener; 

import javax.swing.*; 
import javax.swing.event.PopupMenuEvent; 
import javax.swing.event.PopupMenuListener; 

import net.sf.jabref.groups.*; 

public  class  RightClickMenu  extends JPopupMenu implements  PopupMenuListener {
	

    BasePanel panel;

	
    MetaData metaData;

	
    JMenu groupAddMenu = new JMenu(Globals.lang("Add to group")),
        groupRemoveMenu = new JMenu(Globals.lang("Remove from group")),
        groupMoveMenu = new JMenu("Assign exclusively to group"), 
        typeMenu = new JMenu(Globals.lang("Change entry type"));

	
    JCheckBoxMenuItem
        floatMarked = new JCheckBoxMenuItem(Globals.lang("Float marked entries"),
            Globals.prefs.getBoolean("floatMarkedEntries"));

	

    public RightClickMenu(BasePanel panel_, MetaData metaData_) {
        panel = panel_;
        metaData = metaData_;

        
        boolean multiple = (panel.mainTable.getSelectedRowCount() > 1);

        
        BibtexEntry be = null;
        if (panel.mainTable.getSelectedRowCount() == 1)
          be = panel.mainTable.getSelected().get(0);

        addPopupMenuListener(this);

        add(new AbstractAction(Globals.lang("Copy"), GUIGlobals.getImage("copy")) {
                public void actionPerformed(ActionEvent e) {
                    try {
                        panel.runCommand("copy");
                    } catch (Throwable ex) {}
                }
            });
        add(new AbstractAction(Globals.lang("Paste"), GUIGlobals.getImage("paste")) {
                public void actionPerformed(ActionEvent e) {
                    try {
                        panel.runCommand("paste");
                    } catch (Throwable ex) {}
                }
            });
        add(new AbstractAction(Globals.lang("Cut"), GUIGlobals.getImage("cut")) {
                public void actionPerformed(ActionEvent e) {
                    try {
                        panel.runCommand("cut");
                    } catch (Throwable ex) {}
                }
            });

        add(new AbstractAction(Globals.lang("Delete"), GUIGlobals.getImage("delete")) {
                public void actionPerformed(ActionEvent e) {
                    
                            try {
                                panel.runCommand("delete");
                            } catch (Throwable ex) {}
                        

                }
            });
            addSeparator();

        add(new AbstractAction(Globals.lang("Export to clipboard")) {
                public void actionPerformed(ActionEvent e) {
                    try {
                        panel.runCommand("exportToClipboard");
                    } catch (Throwable ex) {}
                }
            });
            addSeparator();

        if (multiple) {
          add(new AbstractAction(Globals.lang("Mark entries"), GUIGlobals.getImage("markEntries")) {
            public void actionPerformed(ActionEvent e) {
              try {
                panel.runCommand("markEntries");
              } catch (Throwable ex) {}
            }
          });
          add(new AbstractAction(Globals.lang("Unmark entries"), GUIGlobals.getImage("unmarkEntries")) {
            public void actionPerformed(ActionEvent e) {
              try {
                panel.runCommand("unmarkEntries");
              } catch (Throwable ex) {}
            }
          });
          addSeparator();
        } else if (be != null) {
          if (be.getField(BibtexFields.MARKED) == null)
            add(new AbstractAction(Globals.lang("Mark entry"), GUIGlobals.getImage("markEntries")) {
               public void actionPerformed(ActionEvent e) {
                 try {
                   panel.runCommand("markEntries");
                 } catch (Throwable ex) {}
               }
             });
           else
             add(new AbstractAction(Globals.lang("Unmark entry"), GUIGlobals.getImage("unmarkEntries")) {
               public void actionPerformed(ActionEvent e) {
                 try {
                   panel.runCommand("unmarkEntries");
                 } catch (Throwable ex) {}
               }
             });
           addSeparator();
        }

        add(new AbstractAction(Globals.lang("Open file"), GUIGlobals.getImage("openExternalFile")) {
            public void actionPerformed(ActionEvent e) {
                try {
                    panel.runCommand("openExternalFile");
                } catch (Throwable ex) {}
            }
        });

        add(new AbstractAction(Globals.lang("Open PDF or PS"), GUIGlobals.getImage("openFile")) {
                public void actionPerformed(ActionEvent e) {
                    try {
                        panel.runCommand("openFile");
                    } catch (Throwable ex) {}
                }
            });

            add(new AbstractAction(Globals.lang("Open URL or DOI"), GUIGlobals.getImage("www")) {
              public void actionPerformed(ActionEvent e) {
                try {
                  panel.runCommand("openUrl");
                } catch (Throwable ex) {}
              }
            });

        add(new AbstractAction(Globals.lang("Copy BibTeX key")) {
                public void actionPerformed(ActionEvent e) {
                    try {
                        panel.runCommand("copyKey");
                    } catch (Throwable ex) {}
                }
            });

        add(new AbstractAction(Globals.lang("Copy")+" \\cite{"+Globals.lang("BibTeX key")+"}") {
                public void actionPerformed(ActionEvent e) {
                    try {
                        panel.runCommand("copyCiteKey");
                    } catch (Throwable ex) {}
                }
            });

        addSeparator();
        populateTypeMenu();

        add(typeMenu);
        add(new AbstractAction(Globals.lang("Plain text import"))
        {
                public void actionPerformed(ActionEvent e) {
                    try {
                        panel.runCommand("importPlainText");
                    } catch (Throwable ex) {}
                }
            });
        addSeparator(); 

        floatMarked.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                Globals.prefs.putBoolean("floatMarkedEntries", floatMarked.isSelected());
                panel.mainTable.refreshSorting(); 
            }
        });
    }


	

    
    public void populateTypeMenu() {
        typeMenu.removeAll();
        for (String key : BibtexEntryType.ALL_TYPES.keySet()){
            typeMenu.add(new ChangeTypeAction
                             (BibtexEntryType.getType(key), panel));
        }
    }


	

    
    public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
      BibtexEntry[] bes = panel.getSelectedEntries();
      panel.storeCurrentEdit();
      GroupTreeNode groups = metaData.getGroups();
      if (groups == null) {
        groupAddMenu.setEnabled(false);
        groupMoveMenu.setEnabled(false);
        groupRemoveMenu.setEnabled(false);
        return;
      }

      groupAddMenu.setEnabled(true);
      groupMoveMenu.setEnabled(true);
      groupRemoveMenu.setEnabled(true);
      groupAddMenu.removeAll();
      groupMoveMenu.removeAll();
      groupRemoveMenu.removeAll();

      if (bes == null)
        return;
      add(groupAddMenu);
      add(groupMoveMenu);
      add(groupRemoveMenu);

      groupAddMenu.setEnabled(false);
      groupMoveMenu.setEnabled(false);
      groupRemoveMenu.setEnabled(false);
      insertNodes(groupAddMenu,metaData.getGroups(),bes,true,false);
      insertNodes(groupMoveMenu,metaData.getGroups(),bes,true,true);
      insertNodes(groupRemoveMenu,metaData.getGroups(),bes,false,false);

        addSeparator();
        floatMarked.setSelected(Globals.prefs.getBoolean("floatMarkedEntries"));
        add(floatMarked);
    }


	

    
    public void insertNodes(JMenu menu, GroupTreeNode node, BibtexEntry[] selection,
                    boolean add, boolean move) {
        final AbstractAction action = getAction(node,selection,add,move);

        if (node.getChildCount() == 0) {
            JMenuItem menuItem = new JMenuItem(action);
            setGroupFontAndIcon(menuItem, node.getGroup());
            menu.add(menuItem);
            if (action.isEnabled())
                    menu.setEnabled(true);
            return;
        }

        JMenu submenu = null;
        if (node.getGroup() instanceof AllEntriesGroup) {
            for (int i = 0; i < node.getChildCount(); ++i) {
                insertNodes(menu,(GroupTreeNode) node.getChildAt(i), selection, add, move);
            }
        } else {
            submenu = new JMenu("["+node.getGroup().getName()+"]");
            setGroupFontAndIcon(submenu, node.getGroup());
            
            
            submenu.setEnabled(action.isEnabled());
            JMenuItem menuItem = new JMenuItem(action);
            setGroupFontAndIcon(menuItem, node.getGroup());
            submenu.add(menuItem);
            submenu.add(new Separator());
            for (int i = 0; i < node.getChildCount(); ++i)
                insertNodes(submenu,(GroupTreeNode) node.getChildAt(i), selection, add, move);
            menu.add(submenu);
            if (submenu.isEnabled())
                menu.setEnabled(true);
        }
    }


	

    
    private void setGroupFontAndIcon(JMenuItem menuItem, AbstractGroup group) {
        if (Globals.prefs.getBoolean("groupShowDynamic")) {
                menuItem.setFont(menuItem.getFont().deriveFont(group.isDynamic() ?
                                Font.ITALIC : Font.PLAIN));
        }
            if (Globals.prefs.getBoolean("groupShowIcons")) {
                    switch (group.getHierarchicalContext()) {
                    case AbstractGroup.INCLUDING:
                            menuItem.setIcon(GUIGlobals.getImage("groupIncluding"));
                            break;
                    case AbstractGroup.REFINING:
                            menuItem.setIcon(GUIGlobals.getImage("groupRefining"));
                            break;
                    default:
                            menuItem.setIcon(GUIGlobals.getImage("groupRegular"));
                                break;
                    }
            }
    }


	

    
    private AbstractAction getAction(GroupTreeNode node, BibtexEntry[] selection,
                    boolean add, boolean move) {
        AbstractAction action = add ? (AbstractAction) new AddToGroupAction(node, move,
                panel) : (AbstractAction) new RemoveFromGroupAction(node, panel);
        AbstractGroup group = node.getGroup();
        if (!move) {
                action.setEnabled(add ? group.supportsAdd() && !group.containsAll(selection)
                        : group.supportsRemove() && group.containsAny(selection));
        } else {
                action.setEnabled(group.supportsAdd());
        }
        return action;
    }


	

    public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
      remove(groupAddMenu);
      remove(groupMoveMenu);
      remove(groupRemoveMenu);
    }


	

    public void popupMenuCanceled(PopupMenuEvent e) {
        
    }


	

     

    class  ChangeTypeAction  extends AbstractAction {
		
      BibtexEntryType type;

		
      BasePanel panel;

		

      public ChangeTypeAction(BibtexEntryType type, BasePanel bp) {
        super(type.getName());
        this.type = type;
        panel = bp;
      }


		
      public void actionPerformed(ActionEvent evt) {
        panel.changeType(type);
      }



	}


}
