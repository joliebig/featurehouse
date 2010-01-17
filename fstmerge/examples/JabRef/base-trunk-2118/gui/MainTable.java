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


public class MainTable extends JTable {
	
    private MainTableFormat tableFormat;
    private SortedList sortedForMarking, sortedForTable, sortedForSearch, sortedForGrouping;
    private boolean tableColorCodes, showingFloatSearch=false, showingFloatGrouping=false;
    private EventSelectionModel selectionModel;
    private TableComparatorChooser comparatorChooser;
    private JScrollPane pane;
    private Comparator searchComparator, groupComparator,
            markingComparator = new IsMarkedComparator();
    private Matcher searchMatcher, groupMatcher;

    
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


    public MainTable(MainTableFormat tableFormat, EventList list, JabRefFrame frame,
                     BasePanel panel) {
        super();
        this.tableFormat = tableFormat;
        
        
        sortedForTable = new SortedList(list, null);
        
        sortedForMarking = new SortedList(sortedForTable, null);
        
        sortedForSearch = new SortedList(sortedForMarking, null);
        
        sortedForGrouping = new SortedList(sortedForSearch, null);


        searchMatcher = null;
        groupMatcher = null;
        searchComparator = null;
        groupComparator = null;

        EventTableModel tableModel = new EventTableModel(sortedForGrouping, tableFormat);
        setModel(tableModel);

        tableColorCodes = Globals.prefs.getBoolean("tableColorCodesOn");
        selectionModel = new EventSelectionModel(sortedForGrouping);
        setSelectionModel(selectionModel);
        pane = new JScrollPane(this);
        pane.getViewport().setBackground(Globals.prefs.getColor("tableBackground"));
        setGridColor(Globals.prefs.getColor("gridColor"));
        comparatorChooser = new MyTableComparatorChooser(this, sortedForTable,
                TableComparatorChooser.MULTIPLE_COLUMN_KEYBOARD);
        
        final EventList selected = getSelected();

        
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

    
    public void showFloatSearch(Matcher m) {
        showingFloatSearch = true;
        searchMatcher = m;
        searchComparator = new HitOrMissComparator(m);
        refreshSorting();
        scrollTo(0);
    }

    
    public void stopShowingFloatSearch() {
        showingFloatSearch = false;
        searchMatcher = null;
        searchComparator = null;
        refreshSorting();
    }

    
    public void showFloatGrouping(Matcher m) {
        showingFloatGrouping = true;
        groupMatcher = m;
        groupComparator = new HitOrMissComparator(m);
        refreshSorting();
    }

    
    public void stopShowingFloatGrouping() {
        showingFloatGrouping = false;
        groupMatcher = null;
        groupComparator = null;
        refreshSorting();
    }

    public EventList getTableRows() {
        return sortedForGrouping;
    }
    public void addSelectionListener(ListEventListener listener) {
        getSelected().addListEventListener(listener);
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
        return (BibtexEntry)sortedForGrouping.get(row);
    }

    public BibtexEntry[] getSelectedEntries() {
        final BibtexEntry[] BE_ARRAY = new BibtexEntry[0];
        return (BibtexEntry[]) getSelected().toArray(BE_ARRAY);
    }

    
    private void setupComparatorChooser() {
        
        java.util.List comparators = comparatorChooser.getComparatorsForColumn(0);
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
            BibtexEntry be = (BibtexEntry)sortedForGrouping.get(row);
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

    public EventList getSelected() {
        return selectionModel.getSelected();
    }

    public int findEntry(BibtexEntry entry) {
        
        return sortedForGrouping.indexOf(entry);
    }

    public String[] getIconTypeForColumn(int column) {
        return tableFormat.getIconTypeForColumn(column);
    }

    private boolean nonZeroField(int row, String field) {
        BibtexEntry be = (BibtexEntry)sortedForGrouping.get(row);
        Object o = be.getField(field);
        return ((o == null) || !o.equals("0"));
    }

    private boolean matches(int row, Matcher m) {
        Object o = sortedForGrouping.get(row);
        return m.matches(o);
    }

    private boolean isComplete(int row) {
        try {
            BibtexEntry be = (BibtexEntry)sortedForGrouping.get(row);
            return be.hasAllRequiredFields();
        } catch (NullPointerException ex) {
            
            return true;
        }
    }

    private boolean isMarked(int row) {
        try {
            BibtexEntry be = (BibtexEntry)sortedForGrouping.get(row);
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

        boolean antialiasing = Globals.prefs.getBoolean("antialias");
        defRenderer = new GeneralRenderer(Globals.prefs.getColor("tableBackground"),
                Globals.prefs.getColor("tableText"), antialiasing);
        reqRenderer = new GeneralRenderer(Globals.prefs.getColor("tableReqFieldBackground"), Globals.prefs.getColor("tableText"), antialiasing);
        optRenderer = new GeneralRenderer(Globals.prefs.getColor("tableOptFieldBackground"), Globals.prefs.getColor("tableText"), antialiasing);
        incRenderer = new IncompleteRenderer(antialiasing);
        compRenderer = new CompleteRenderer(Globals.prefs.getColor("tableBackground"), antialiasing);
        markedNumberRenderer = new CompleteRenderer(Globals.prefs.getColor("markedEntryBackground"), antialiasing);
        grayedOutNumberRenderer = new CompleteRenderer(Globals.prefs.getColor("grayedOutBackground"), antialiasing);
        veryGrayedOutNumberRenderer = new CompleteRenderer(Globals.prefs.getColor("veryGrayedOutBackground"), antialiasing);
        grayedOutRenderer = new GeneralRenderer(Globals.prefs.getColor("grayedOutBackground"),
            Globals.prefs.getColor("grayedOutText"), antialiasing);
        veryGrayedOutRenderer = new GeneralRenderer(Globals.prefs.getColor("veryGrayedOutBackground"),
                Globals.prefs.getColor("veryGrayedOutText"), antialiasing);
        markedRenderer = new GeneralRenderer(Globals.prefs.getColor("markedEntryBackground"),
                Globals.prefs.getColor("tableText"), antialiasing);
    }

    static class IncompleteRenderer extends GeneralRenderer {
        public IncompleteRenderer(boolean antialiasing) {
            super(Globals.prefs.getColor("incompleteEntryBackground"), antialiasing);
            super.setToolTipText(Globals.lang("This entry is incomplete"));
        }

        protected void setNumber(int number) {
            super.setValue(String.valueOf(number + 1));
        }

        protected void setValue(Object value) {

        }
    }

    static class CompleteRenderer extends GeneralRenderer {
        public CompleteRenderer(Color color, boolean antialiasing) {
            super(color, antialiasing);
        }

        protected void setNumber(int number) {
            super.setValue(String.valueOf(number + 1));
        }

        protected void setValue(Object value) {

        }
    }

    class MyTableComparatorChooser extends TableComparatorChooser {
        public MyTableComparatorChooser(JTable table, SortedList list,
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

    
    public Comparator getComparatorForColumn(int index) {
        java.util.List l = comparatorChooser.getComparatorsForColumn(index);
        return l.size() == 0 ? null : (Comparator)l.get(0);
    }

    
    public int getSortingColumn(int number) {
        java.util.List l = comparatorChooser.getSortingColumns();
        if (l.size() <= number)
            return -1;
        else
            return ((Integer)l.get(number)).intValue();
    }

    
    public SortedList getSortedForTable() {
        return sortedForTable;
    }
}
