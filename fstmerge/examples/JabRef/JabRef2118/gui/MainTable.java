package net.sf.jabref.gui; 

import net.sf.jabref.*; 
import net.sf.jabref.search.SearchMatcher; 
import net.sf.jabref.search.HitOrMissComparator; 
import net.sf.jabref.groups.EntryTableTransferHandler; 

import javax.swing.*; 
import javax.swing.plaf.TableUI; 
import javax.swing.table.TableCellRenderer; 
import javax.swing.table.TableModel; 
import javax.swing.table.TableColumnModel; 
import ca.odell.glazedlists.SortedList; 
import ca.odell.glazedlists.EventList; 
import ca.odell.glazedlists.gui.AbstractTableComparatorChooser; 
import ca.odell.glazedlists.matchers.Matcher; 
import ca.odell.glazedlists.event.ListEventListener; 
import ca.odell.glazedlists.swing.EventSelectionModel; 
import ca.odell.glazedlists.swing.TableComparatorChooser; 
import ca.odell.glazedlists.swing.EventTableModel; 

import java.awt.*; 
import java.awt.event.ActionListener; 
import java.awt.event.ActionEvent; 
import java.util.Comparator; 

import java.awt.Color; 
import java.awt.Rectangle; 
import java.util.List; 


public  class  MainTable  extends JTable {
	
	
    private MainTableFormat tableFormat;

	
    

	
    private boolean tableColorCodes, showingFloatSearch=false, showingFloatGrouping=false;

	
    

	
    

	
    private JScrollPane pane;

	
    

	
    

	

    
    public static final int REQUIRED = 1, OPTIONAL = 2,
      REQ_STRING = 1,
      REQ_NUMBER = 2,
      OPT_STRING = 3,
      OTHER = 3,
      BOOLEAN = 4,
      ICON_COL = 8;

	 

    static {
        updateRenderers();
    }

	


    public MainTable(MainTableFormat tableFormat, EventList<BibtexEntry> list, JabRefFrame frame,
                     BasePanel panel) {
        super();
        this.tableFormat = tableFormat;
        
        
        sortedForTable = new SortedList<BibtexEntry>(list, null);
        
        sortedForMarking = new SortedList<BibtexEntry>(sortedForTable, null);
        
        sortedForSearch = new SortedList<BibtexEntry>(sortedForMarking, null);
        
        sortedForGrouping = new SortedList<BibtexEntry>(sortedForSearch, null);


        searchMatcher = null;
        groupMatcher = null;
        searchComparator = null;
        groupComparator = null;

        EventTableModel<BibtexEntry> tableModel = new EventTableModel<BibtexEntry>(sortedForGrouping, tableFormat);
        setModel(tableModel);

        tableColorCodes = Globals.prefs.getBoolean("tableColorCodesOn");
        selectionModel = new EventSelectionModel<BibtexEntry>(sortedForGrouping);
        setSelectionModel(selectionModel);
        pane = new JScrollPane(this);
        pane.getViewport().setBackground(Globals.prefs.getColor("tableBackground"));
        setGridColor(Globals.prefs.getColor("gridColor"));
        comparatorChooser = new MyTableComparatorChooser(this, sortedForTable,
                TableComparatorChooser.MULTIPLE_COLUMN_KEYBOARD);
        
        
        getSelected();

        
        setDragEnabled(true);
        TransferHandler xfer = new EntryTableTransferHandler(this, frame, panel);
        setTransferHandler(xfer);
        pane.setTransferHandler(xfer);

        setupComparatorChooser();
        refreshSorting();
        setWidths();

    }


	

    public void refreshSorting() {
        sortedForMarking.getReadWriteLock().writeLock().lock();
        if (Globals.prefs.getBoolean("floatMarkedEntries"))
            sortedForMarking.setComparator(markingComparator);
        else
            sortedForMarking.setComparator(null);
        sortedForMarking.getReadWriteLock().writeLock().unlock();
        sortedForSearch.getReadWriteLock().writeLock().lock();
        sortedForSearch.setComparator(searchComparator);
        sortedForSearch.getReadWriteLock().writeLock().unlock();
        sortedForGrouping.getReadWriteLock().writeLock().lock();
        sortedForGrouping.setComparator(groupComparator);
        sortedForGrouping.getReadWriteLock().writeLock().unlock();
    }


	

    
    


	

    
    public void stopShowingFloatSearch() {
        showingFloatSearch = false;
        searchMatcher = null;
        searchComparator = null;
        refreshSorting();
    }


	

    
    


	

    
    public void stopShowingFloatGrouping() {
        showingFloatGrouping = false;
        groupMatcher = null;
        groupComparator = null;
        refreshSorting();
    }


	

    public EventList<BibtexEntry> getTableRows() {
        return sortedForGrouping;
    }


	
    


	

    public JScrollPane getPane() {
        return pane;
    }


	

    public TableCellRenderer getCellRenderer(int row, int column) {

        int score = -3;
        TableCellRenderer renderer = defRenderer;

        int status = getCellStatus(row, column);

        if (!showingFloatSearch || matches(row, searchMatcher))
            score++;
        if (!showingFloatGrouping || matches(row, groupMatcher))
            score += 2;

        
        
        if (score < -1) {
            if (column == 0) {
                veryGrayedOutNumberRenderer.setNumber(row);
                renderer = veryGrayedOutNumberRenderer;
            } else renderer = veryGrayedOutRenderer;
        }
        else if (score == -1) {
            if (column == 0) {
                grayedOutNumberRenderer.setNumber(row);
                renderer = grayedOutNumberRenderer;
            } else renderer = grayedOutRenderer;
        }

        else if (column == 0) {
            
            if (!isComplete(row)) {
                incRenderer.setNumber(row);
                renderer = incRenderer;
            } else {
                compRenderer.setNumber(row);
                if (isMarked(row)) {
                    renderer = markedNumberRenderer;
                    markedNumberRenderer.setNumber(row);
                } else
                    renderer = compRenderer;
            }
        }
        else if (tableColorCodes) {
            if (status == REQUIRED)
                renderer = reqRenderer;
            else if (status == OPTIONAL)
                renderer = optRenderer;
            else if (status == BOOLEAN)
                renderer = getDefaultRenderer(Boolean.class);
        }

        
        if ((column != 0) && isMarked(row)) {
            renderer = markedRenderer;
        }

        return renderer;

    }


	

    public void setWidths() {
        
        int ncWidth = Globals.prefs.getInt("numberColWidth");
        String[] widths = Globals.prefs.getStringArray("columnWidths");
        TableColumnModel cm = getColumnModel();
        cm.getColumn(0).setPreferredWidth(ncWidth);
        for (int i = 1; i < tableFormat.padleft; i++) {
            
            cm.getColumn(i).setPreferredWidth(GUIGlobals.WIDTH_ICON_COL);
            cm.getColumn(i).setMinWidth(GUIGlobals.WIDTH_ICON_COL);
            cm.getColumn(i).setMaxWidth(GUIGlobals.WIDTH_ICON_COL);
        }
        for (int i = tableFormat.padleft; i < getModel().getColumnCount(); i++) {
            try {
                cm.getColumn(i).setPreferredWidth(Integer.parseInt(widths[i - tableFormat.padleft]));
            } catch (Throwable ex) {
                Globals.logger("Exception while setting column widths. Choosing default.");
                cm.getColumn(i).setPreferredWidth(GUIGlobals.DEFAULT_FIELD_LENGTH);
            }

        }
    }


	

    public BibtexEntry getEntryAt(int row) {
        return sortedForGrouping.get(row);
    }


	

    public BibtexEntry[] getSelectedEntries() {
        final BibtexEntry[] BE_ARRAY = new BibtexEntry[0];
        return getSelected().toArray(BE_ARRAY);
    }


	

    
    @SuppressWarnings("unchecked")
	private void setupComparatorChooser() {
        
        List<Comparator<BibtexEntry>> comparators = comparatorChooser.getComparatorsForColumn(0);
        comparators.clear();
        comparators.add(new FirstColumnComparator());

        
        for (int i = 1; i < tableFormat.padleft; i++) {
            comparators = comparatorChooser.getComparatorsForColumn(i);
            comparators.clear();
            String[] iconField = tableFormat.getIconTypeForColumn(i);
            comparators.add(new IconComparator(iconField));
        }
        
        for (int i = tableFormat.padleft; i < tableFormat.getColumnCount(); i++) {
            comparators = comparatorChooser.getComparatorsForColumn(i);
            comparators.clear();
            comparators.add(new FieldComparator(tableFormat.getColumnName(i).toLowerCase()));
        }

        

        
        String[] sortFields = new String[] {Globals.prefs.get("priSort"), Globals.prefs.get("secSort"),
            Globals.prefs.get("terSort")};
        boolean[] sortDirections = new boolean[] {Globals.prefs.getBoolean("priDescending"),
            Globals.prefs.getBoolean("secDescending"), Globals.prefs.getBoolean("terDescending")}; 

        sortedForTable.getReadWriteLock().writeLock().lock();
        for (int i=0; i<sortFields.length; i++) {
            int index = tableFormat.getColumnIndex(sortFields[i]);
            if (index >= 0) {
                comparatorChooser.appendComparator(index, 0, sortDirections[i]);
            }
        }
        sortedForTable.getReadWriteLock().writeLock().unlock();

    }


	

    public int getCellStatus(int row, int col) {
        try {
            BibtexEntry be = sortedForGrouping.get(row);
            BibtexEntryType type = be.getType();
            String columnName = tableFormat.getColumnName(col).toLowerCase();
            if (columnName.equals(BibtexFields.KEY_FIELD) || type.isRequired(columnName)) {
                return REQUIRED;
            }
            if (type.isOptional(columnName)) {
                return OPTIONAL;
            }
            return OTHER;
        } catch (NullPointerException ex) {
            
            return OTHER;
        }
    }


	

    public EventList<BibtexEntry> getSelected() {
        return selectionModel.getSelected();
    }


	

    public int findEntry(BibtexEntry entry) {
        
        return sortedForGrouping.indexOf(entry);
    }


	

    public String[] getIconTypeForColumn(int column) {
        return tableFormat.getIconTypeForColumn(column);
    }


	

    


	

    


	

    private boolean isComplete(int row) {
        try {
            BibtexEntry be = sortedForGrouping.get(row);
            return be.hasAllRequiredFields();
        } catch (NullPointerException ex) {
            
            return true;
        }
    }


	

    private boolean isMarked(int row) {
        try {
            BibtexEntry be = sortedForGrouping.get(row);
            return Util.isMarked(be);
        } catch (NullPointerException ex) {
            
            return false;
        }
    }


	


    public void scrollTo(int y) {
        JScrollBar scb = pane.getVerticalScrollBar();
        scb.setValue(y * scb.getUnitIncrement(1));
    }


	

    
    public void updateFont() {
        setFont(GUIGlobals.CURRENTFONT);
        setRowHeight(GUIGlobals.TABLE_ROW_PADDING + GUIGlobals.CURRENTFONT.getSize());
    }


	

    public void ensureVisible(int row) {
        JScrollBar vert = pane.getVerticalScrollBar();
        int y = row * getRowHeight();
        if ((y < vert.getValue()) || (y > vert.getValue() + vert.getVisibleAmount()))
            scrollToCenter(row, 1);
    }


	

    public void scrollToCenter(int rowIndex, int vColIndex) {
        if (!(this.getParent() instanceof JViewport)) {
            return;
        }

        JViewport viewport = (JViewport) this.getParent();

        
        
        Rectangle rect = this.getCellRect(rowIndex, vColIndex, true);

        
        Rectangle viewRect = viewport.getViewRect();

        
        
        
        rect.setLocation(rect.x - viewRect.x, rect.y - viewRect.y);

        
        int centerX = (viewRect.width - rect.width) / 2;
        int centerY = (viewRect.height - rect.height) / 2;

        
        
        if (rect.x < centerX) {
            centerX = -centerX;
        }
        if (rect.y < centerY) {
            centerY = -centerY;
        }
        rect.translate(centerX, centerY);

        
        viewport.scrollRectToVisible(rect);

        revalidate();
        repaint();
    }


	


    private static GeneralRenderer defRenderer
    ,
    reqRenderer
    ,
    optRenderer
    ,
    grayedOutRenderer,
    veryGrayedOutRenderer
    ,
    markedRenderer;

	

    private static IncompleteRenderer incRenderer;

	
    private static CompleteRenderer
            compRenderer,
            grayedOutNumberRenderer,
            veryGrayedOutNumberRenderer,
            markedNumberRenderer;

	

    public static void updateRenderers() {

        defRenderer = new GeneralRenderer(Globals.prefs.getColor("tableBackground"),
                Globals.prefs.getColor("tableText"));
        reqRenderer = new GeneralRenderer(Globals.prefs.getColor("tableReqFieldBackground"), Globals.prefs.getColor("tableText"));
        optRenderer = new GeneralRenderer(Globals.prefs.getColor("tableOptFieldBackground"), Globals.prefs.getColor("tableText"));
        incRenderer = new IncompleteRenderer();
        compRenderer = new CompleteRenderer(Globals.prefs.getColor("tableBackground"));
        markedNumberRenderer = new CompleteRenderer(Globals.prefs.getColor("markedEntryBackground"));
        grayedOutNumberRenderer = new CompleteRenderer(Globals.prefs.getColor("grayedOutBackground"));
        veryGrayedOutNumberRenderer = new CompleteRenderer(Globals.prefs.getColor("veryGrayedOutBackground"));
        grayedOutRenderer = new GeneralRenderer(Globals.prefs.getColor("grayedOutBackground"),
            Globals.prefs.getColor("grayedOutText"));
        veryGrayedOutRenderer = new GeneralRenderer(Globals.prefs.getColor("veryGrayedOutBackground"),
                Globals.prefs.getColor("veryGrayedOutText"));
        markedRenderer = new GeneralRenderer(Globals.prefs.getColor("markedEntryBackground"),
                Globals.prefs.getColor("tableText"));
    }


	

    static  class  IncompleteRenderer  extends GeneralRenderer {
		
        public IncompleteRenderer() {
            super(Globals.prefs.getColor("incompleteEntryBackground"));
            super.setToolTipText(Globals.lang("This entry is incomplete"));
        }


		

        protected void setNumber(int number) {
            super.setValue(String.valueOf(number + 1));
        }


		

        protected void setValue(Object value) {

        }



	}

	

    static  class  CompleteRenderer  extends GeneralRenderer {
		
        public CompleteRenderer(Color color) {
            super(color);
        }


		

        protected void setNumber(int number) {
            super.setValue(String.valueOf(number + 1));
        }


		

        protected void setValue(Object value) {

        }



	}

	

     

    class  MyTableComparatorChooser  extends TableComparatorChooser<BibtexEntry> {
		
        public MyTableComparatorChooser(JTable table, SortedList<BibtexEntry> list,
                                        Object sortingStrategy) {
            super(table, list, sortingStrategy);
            
            
            addSortActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    
                    refreshSorting();
                }
            });
        }



	}

	

    
    public void setUI(TableUI newUI) {
        super.setUI(newUI);
        TransferHandler handler = getTransferHandler();
        setTransferHandler(null);
        setTransferHandler(handler);

    }


	

    
    @SuppressWarnings("unchecked")
	public Comparator<BibtexEntry> getComparatorForColumn(int index) {
        List<Comparator<BibtexEntry>> l = comparatorChooser.getComparatorsForColumn(index);
        return l.size() == 0 ? null : l.get(0);
    }


	

    
    public int getSortingColumn(int number) {
        List<Integer> l = comparatorChooser.getSortingColumns();
        if (l.size() <= number)
            return -1;
        else
            return ((Integer)l.get(number)).intValue();
    }


	

    
    public SortedList<BibtexEntry> getSortedForTable() {
        return sortedForTable;
    }


	
    private SortedList<BibtexEntry> sortedForMarking, sortedForTable, sortedForSearch, sortedForGrouping;

	
    private EventSelectionModel<BibtexEntry> selectionModel;

	
    private TableComparatorChooser<BibtexEntry> comparatorChooser;

	
    private Comparator<BibtexEntry> searchComparator, groupComparator,
            markingComparator = new IsMarkedComparator();

	
    private Matcher<BibtexEntry> searchMatcher, groupMatcher;

	 

    static {
        updateRenderers();
    }

	

    
    public void showFloatSearch(Matcher<BibtexEntry> m) {
        showingFloatSearch = true;
        searchMatcher = m;
        searchComparator = new HitOrMissComparator(m);
        refreshSorting();
        scrollTo(0);
    }

	

    
    public void showFloatGrouping(Matcher<BibtexEntry> m) {
        showingFloatGrouping = true;
        groupMatcher = m;
        groupComparator = new HitOrMissComparator(m);
        refreshSorting();
    }

	
    public void addSelectionListener(ListEventListener<BibtexEntry> listener) {
        getSelected().addListEventListener(listener);
    }

	

    private boolean matches(int row, Matcher<BibtexEntry> m) {
        return m.matches(sortedForGrouping.get(row));
    }


}
