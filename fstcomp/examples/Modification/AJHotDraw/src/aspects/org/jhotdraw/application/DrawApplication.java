
package org.jhotdraw.application; 
import java.awt.*; 
import java.awt.event.WindowAdapter; 
import java.awt.event.WindowEvent; 
import java.io.IOException; 
import java.util.ListIterator; 
import javax.swing.*; 
import org.jhotdraw.contrib.*; 
import org.jhotdraw.contrib.Desktop; 
import org.jhotdraw.figures.*; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.util.*; 
public 	class  DrawApplication 	extends JFrame 	implements DrawingEditor, PaletteListener, VersionRequester {
		private Tool	fTool;

		private Iconkit	fIconkit;

		private JTextField	fStatusLine;

		private DrawingView	fView;

		private ToolButton	fDefaultToolButton;

		private ToolButton	fSelectedToolButton;

		private String	fApplicationName;

		private StorageFormatManager	fStorageFormatManager;

		private UndoManager	myUndoManager;

		protected static String	fgUntitled = "untitled";

		private java.util.List	listeners;

		private DesktopListener fDesktopListener;

		private Desktop fDesktop;

		private static final String	fgDrawPath = "/org/jhotdraw/";

		public static final String	IMAGES = fgDrawPath + "images/";

		protected static int	winCount = 0;

		public static final int	FILE_MENU = 0;

		public static final int	EDIT_MENU = 1;

		public static final int	ALIGNMENT_MENU = 2;

		public static final int	ATTRIBUTES_MENU = 3;

		public DrawApplication() {	this("AJHotDraw");	}

		public DrawApplication(String title) {	super(title);	listeners = CollectionsFactory.current().createList();	setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);	setApplicationName(title);	}

		protected DrawApplication createApplication() {	return new DrawApplication();	}

		public void newView() {	if (view() == null) {	return;	}	DrawApplication window = createApplication();	window.open(view());	if (view().drawing().getTitle() != null ) {	window.setDrawingTitle(view().drawing().getTitle() + " (View)");	}	else {	window.setDrawingTitle(getDefaultDrawingTitle() + " (View)");	}	}

		public void newWindow(Drawing initialDrawing) {	DrawApplication window = createApplication();	if (initialDrawing == null) {	window.open();	}	else {	window.open(window.createDrawingView(initialDrawing));	}	}

		public final void newWindow() { newWindow(createDrawing());	}

		public void open() {	open(createInitialDrawingView());	}

		protected void open(final DrawingView newDrawingView) {	getVersionControlStrategy().assertCompatibleVersion();	setUndoManager(new UndoManager());	setIconkit(createIconkit());	getContentPane().setLayout(new BorderLayout());	setStatusLine(createStatusLine());	getContentPane().add(getStatusLine(), BorderLayout.SOUTH);	setTool(new NullTool(this), "");	setView(newDrawingView);	JToolBar tools = createToolPalette();	createTools(tools);	JPanel activePanel = new JPanel();	activePanel.setAlignmentX(LEFT_ALIGNMENT);	activePanel.setAlignmentY(TOP_ALIGNMENT);	activePanel.setLayout(new BorderLayout());	activePanel.add(tools, BorderLayout.NORTH);	setDesktopListener(createDesktopListener());	setDesktop(createDesktop());	activePanel.add((Component)getDesktop(), BorderLayout.CENTER);	getContentPane().add(activePanel, BorderLayout.CENTER);	JMenuBar mb = new JMenuBar();	createMenus(mb);	setJMenuBar(mb);	Dimension d = defaultSize();	if (d.width > mb.getPreferredSize().width) {	setSize(d.width, d.height);	}	else {	setSize(mb.getPreferredSize().width, d.height);	}	addListeners();	setStorageFormatManager(createStorageFormatManager());	setVisible(true);	Runnable r = new Runnable() {	public void run() {	if (newDrawingView.isInteractive()) {	getDesktop().addToDesktop(newDrawingView , Desktop.PRIMARY);	}	toolDone();	}	};	if (java.awt.EventQueue.isDispatchThread() == false) {	try {	java.awt.EventQueue.invokeAndWait(r);	}	catch(java.lang.InterruptedException ie) {	System.err.println(ie.getMessage());	exit();	}	catch(java.lang.reflect.InvocationTargetException ite) {	System.err.println(ite.getMessage());	exit();	}	}	else {	r.run();	}	toolDone();	}

		protected void addListeners() {	addWindowListener(	new WindowAdapter() {	public void windowClosing(WindowEvent event) {	endApp();	}	public void windowOpened(WindowEvent event) {	winCount++;	}	public void windowClosed(WindowEvent event) {	if (--winCount == 0) {	System.exit(0);	}	}	}	);	}

		protected void createMenus(JMenuBar mb) {	addMenuIfPossible(mb, createFileMenu());	addMenuIfPossible(mb, createEditMenu());	addMenuIfPossible(mb, createAlignmentMenu());	addMenuIfPossible(mb, createAttributesMenu());	addMenuIfPossible(mb, createDebugMenu());	}

		protected void addMenuIfPossible(JMenuBar mb, JMenu newMenu) {	if (newMenu != null) {	mb.add(newMenu);	}	}

		protected JMenu createFileMenu() {	CommandMenu menu = new CommandMenu("File");	Command cmd = new AbstractCommand("New", this, false) {	public void execute() {	promptNew();	}	};	menu.add(cmd, new MenuShortcut('n'));	cmd = new AbstractCommand("Open...", this, false) {	public void execute() {	promptOpen();	}	};	menu.add(cmd, new MenuShortcut('o'));	cmd = new AbstractCommand("Save As...", this, true) {	public void execute() {	promptSaveAs();	}	};	menu.add(cmd, new MenuShortcut('s'));	menu.addSeparator();	cmd = new AbstractCommand("Print...", this, true) {	public void execute() {	print();	}	};	menu.add(cmd, new MenuShortcut('p'));	menu.addSeparator();	cmd = new AbstractCommand("Exit", this, true) {	public void execute() {	endApp();	}	};	menu.add(cmd);	return menu;	}

		protected JMenu createEditMenu() {	CommandMenu menu = new CommandMenu("Edit");	menu.add(	new SelectAllCommand("Select All", this)/*)*/, new MenuShortcut('a'));	menu.addSeparator();	menu.add(	new CutCommand("Cut", this)/*)*/, new MenuShortcut('x'));	menu.add(new CopyCommand("Copy", this), new MenuShortcut('c'));	menu.add(	new PasteCommand("Paste", this)/*)*/, new MenuShortcut('v'));	menu.addSeparator();	menu.add(	new DuplicateCommand("Duplicate", this)/*)*/, new MenuShortcut('d'));	menu.add(/*@AJHD new UndoableCommand(*/new DeleteCommand("Delete", this)/*)*/);	menu.addSeparator();	menu.add(/*@AJHD new UndoableCommand(*/new GroupCommand("Group", this)/*)*/);	menu.add(/*@AJHD new UndoableCommand(*/new UngroupCommand("Ungroup", this)/*)*/);	menu.addSeparator();	menu.add(/*@AJHD new UndoableCommand(*/new SendToBackCommand("Send to Back", this)/*)*/);	menu.add(/*@AJHD new UndoableCommand(*/new BringToFrontCommand("Bring to Front", this)/*)*/);	menu.addSeparator();	menu.add(new UndoCommand("Undo Command", this));	menu.add(new RedoCommand("Redo Command", this));	return menu;	}

		protected JMenu createAlignmentMenu() {	CommandMenu menu = new CommandMenu("Align");	menu.addCheckItem(new ToggleGridCommand("Toggle Snap to Grid", this, new Point(4,4)));	menu.addSeparator();	menu.add(	new AlignCommand(AlignCommand.Alignment.LEFTS, this)/*)*/);	menu.add(	new AlignCommand(AlignCommand.Alignment.CENTERS, this)/*)*/);	menu.add(	new AlignCommand(AlignCommand.Alignment.RIGHTS, this)/*)*/);	menu.addSeparator();	menu.add(	new AlignCommand(AlignCommand.Alignment.TOPS, this)/*)*/);	menu.add(	new AlignCommand(AlignCommand.Alignment.MIDDLES, this)/*)*/);	menu.add(	new AlignCommand(AlignCommand.Alignment.BOTTOMS, this)/*)*/);	return menu;	}

		protected JMenu createDebugMenu() {	CommandMenu menu = new CommandMenu("Debug");	Command cmd = new AbstractCommand("Simple Update", this) {	public void execute() {	this.view().setDisplayUpdate(new SimpleUpdateStrategy());	}	};	menu.add(cmd);	cmd = new AbstractCommand("Buffered Update", this) {	public void execute() {	this.view().setDisplayUpdate(new BufferedUpdateStrategy());	}	};	menu.add(cmd);	return menu;	}

		protected JMenu createAttributesMenu() {	JMenu menu = new JMenu("Attributes");	menu.add(createColorMenu("Fill Color", FigureAttributeConstant.FILL_COLOR));	menu.add(createColorMenu("Pen Color", FigureAttributeConstant.FRAME_COLOR));	menu.add(createArrowMenu());	menu.addSeparator();	menu.add(createFontMenu());	menu.add(createFontSizeMenu());	menu.add(createFontStyleMenu());	menu.add(createColorMenu("Text Color", FigureAttributeConstant.TEXT_COLOR));	return menu;	}

		protected JMenu createColorMenu(String title, FigureAttributeConstant attribute) {	CommandMenu menu = new CommandMenu(title);	for (int i=0; i<ColorMap.size(); i++)	menu.add(	new ChangeAttributeCommand(	ColorMap.name(i),	attribute,	ColorMap.color(i),	this	)	);	return menu;	}

		protected JMenu createArrowMenu() {	FigureAttributeConstant arrowMode = FigureAttributeConstant.ARROW_MODE;	CommandMenu menu = new CommandMenu("Arrow");	menu.add(	new ChangeAttributeCommand("none", arrowMode, new Integer(PolyLineFigure.ARROW_TIP_NONE), this)/*)*/);	menu.add(	new ChangeAttributeCommand("at Start", arrowMode, new Integer(PolyLineFigure.ARROW_TIP_START), this)/*)*/);	menu.add(	new ChangeAttributeCommand("at End", arrowMode, new Integer(PolyLineFigure.ARROW_TIP_END), this)/*)*/);	menu.add(	new ChangeAttributeCommand("at Both", arrowMode, new Integer(PolyLineFigure.ARROW_TIP_BOTH), this)/*)*/);	return menu;	}

		protected JMenu createFontMenu() {	CommandMenu menu = new CommandMenu("Font");	String fonts[] = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();	for (int i = 0; i < fonts.length; i++) {	menu.add(	new ChangeAttributeCommand(fonts[i], FigureAttributeConstant.FONT_NAME, fonts[i], this)/*)*/);	}	return menu;	}

		protected JMenu createFontStyleMenu() {	FigureAttributeConstant fontStyle = FigureAttributeConstant.FONT_STYLE;	CommandMenu menu = new CommandMenu("Font Style");	menu.add(	new ChangeAttributeCommand("Plain", fontStyle, new Integer(Font.PLAIN), this)/*)*/);	menu.add(	new ChangeAttributeCommand("Italic", fontStyle, new Integer(Font.ITALIC), this)/*)*/);	menu.add(	new ChangeAttributeCommand("Bold", fontStyle, new Integer(Font.BOLD), this)/*)*/);	return menu;	}

		protected JMenu createFontSizeMenu() {	CommandMenu menu = new CommandMenu("Font Size");	int sizes[] = { 9, 10, 12, 14, 18, 24, 36, 48, 72 };	for (int i = 0; i < sizes.length; i++) { menu.add(	new ChangeAttributeCommand(	Integer.toString(sizes[i]),	FigureAttributeConstant.FONT_SIZE,	new Integer(sizes[i]),	this	)	);	}	return menu;	}

		public JMenu createLookAndFeelMenu() {	CommandMenu menu = new CommandMenu("Look'n'Feel");	UIManager.LookAndFeelInfo[] lafs = UIManager.getInstalledLookAndFeels();	for (int i = 0; i < lafs.length; i++) {	final String lnfClassName = lafs[i].getClassName();	Command cmd = new AbstractCommand(lafs[i].getName(), this) {	public void execute() {	newLookAndFeel(lnfClassName);	}	};	menu.add(cmd);	}	return menu;	}

		protected JToolBar createToolPalette() {	JToolBar palette = new JToolBar();	palette.setBackground(Color.lightGray);	return palette;	}

		protected void createTools(JToolBar palette) {	setDefaultTool(createDefaultTool());	palette.add(fDefaultToolButton);	}

		protected Tool createSelectionTool() {	return new SelectionTool(this);	}

		protected Tool createDefaultTool() {	return createSelectionTool();	}

		protected void setDefaultTool(Tool newDefaultTool) {	if (newDefaultTool != null) {	fDefaultToolButton = createToolButton(IMAGES+"SEL", "Selection Tool", newDefaultTool);	}	else {	fDefaultToolButton = null;	}	}

		public Tool getDefaultTool() {	if (fDefaultToolButton != null) {	return fDefaultToolButton.tool();	}	else {	return null;	}	}

		protected ToolButton createToolButton(String iconName, String toolName, Tool tool) {	return new ToolButton(this, iconName, toolName, tool);	}

		protected DrawingView createDrawingView() {	DrawingView createdDrawingView = createDrawingView(createDrawing());	createdDrawingView.drawing().setTitle(getDefaultDrawingTitle());	return createdDrawingView;	}

		protected DrawingView createDrawingView(Drawing newDrawing) {	Dimension d = getDrawingViewSize();	DrawingView newDrawingView = new StandardDrawingView(this, d.width, d.height);	newDrawingView.setDrawing(newDrawing);	return newDrawingView;	}

		protected DrawingView createInitialDrawingView() {	return createDrawingView();	}

		protected Dimension getDrawingViewSize() {	return new Dimension(800, 800);	}

		protected Drawing createDrawing() {	return new StandardDrawing();	}

		protected Desktop createDesktop() {	return new JPanelDesktop(this);	}

		protected void setDesktop(Desktop newDesktop) {	newDesktop.addDesktopListener(getDesktopListener());	fDesktop = newDesktop;	}

		public Desktop getDesktop() {	return fDesktop;	}

		public StorageFormatManager createStorageFormatManager() {	StorageFormatManager storageFormatManager = new StorageFormatManager();	storageFormatManager.setDefaultStorageFormat(new StandardStorageFormat());	storageFormatManager.addStorageFormat(storageFormatManager.getDefaultStorageFormat());	storageFormatManager.addStorageFormat(new SerializationStorageFormat());	return storageFormatManager;	}

		protected final void setStorageFormatManager(StorageFormatManager newStorageFormatManager) {	fStorageFormatManager = newStorageFormatManager;	}

		public StorageFormatManager getStorageFormatManager() {	return fStorageFormatManager;	}

		protected Dimension defaultSize() {	return new Dimension(600,450);	}

		protected JTextField createStatusLine() {	JTextField field = new JTextField("No Tool", 40);	field.setBackground(Color.white);	field.setEditable(false);	return field;	}

		private void setStatusLine(JTextField newStatusLine) {	fStatusLine = newStatusLine;	}

		protected JTextField getStatusLine() {	return fStatusLine;	}

		public void paletteUserSelected(PaletteButton paletteButton) {	ToolButton toolButton = (ToolButton)paletteButton;	setTool(toolButton.tool(), toolButton.name());	setSelected(toolButton);	}

		public void paletteUserOver(PaletteButton paletteButton, boolean inside) {	ToolButton toolButton = (ToolButton)paletteButton;	if (inside) {	showStatus(toolButton.name());	}	else if (fSelectedToolButton != null) {	showStatus(fSelectedToolButton.name());	}	}

		public Tool tool() {	return fTool;	}

		public DrawingView view() {	return fView;	}

		protected void setView(DrawingView newView) {	DrawingView oldView = fView;	fView = newView;	fireViewSelectionChangedEvent(oldView, view());	}

		public DrawingView[] views() {	return new DrawingView[] { view() };	}

		public void toolDone() {	System.out.println("ToolDone");	if (fDefaultToolButton != null) {	setTool(fDefaultToolButton.tool(), fDefaultToolButton.name());	setSelected(fDefaultToolButton);	}	}

		protected void checkCommandMenus() {	JMenuBar mb = getJMenuBar();	for (int x = 0; x < mb.getMenuCount(); x++) { JMenu jm = mb.getMenu(x);	if (CommandMenu.class.isInstance(jm)) {	checkCommandMenu((CommandMenu)jm);	}	}	}

		protected void checkCommandMenu(CommandMenu cm) {	cm.checkEnabled();	for (int y = 0; y < cm.getItemCount();y++) {	JMenuItem jmi = cm.getItem(y);	if (CommandMenu.class.isInstance(jmi)) {	checkCommandMenu((CommandMenu)jmi);	}	}	}

		public void addViewChangeListener(ViewChangeListener vsl) {	listeners.add(vsl);	}

		public void removeViewChangeListener(ViewChangeListener vsl) {	listeners.remove(vsl);	}

		protected void fireViewSelectionChangedEvent(DrawingView oldView, DrawingView newView) {	ListIterator li= listeners.listIterator(listeners.size());	while (li.hasPrevious()) {	ViewChangeListener vsl = (ViewChangeListener)li.previous();	vsl.viewSelectionChanged(oldView, newView);	}	}

		protected void fireViewCreatedEvent(DrawingView view) {	ListIterator li= listeners.listIterator(listeners.size());	while (li.hasPrevious()) {	ViewChangeListener vsl = (ViewChangeListener)li.previous();	vsl.viewCreated(view);	}	}

		protected void fireViewDestroyingEvent(DrawingView view) {	ListIterator li= listeners.listIterator(listeners.size());	while (li.hasPrevious()) {	ViewChangeListener vsl = (ViewChangeListener)li.previous();	vsl.viewDestroying( view );	}	}

		public void showStatus(String string) {	getStatusLine().setText(string);	}

		public void setTool(Tool t, String name) {	if ((tool() != null) && (tool().isActive())) {	tool().deactivate();	}	fTool = t;	if (tool() != null) {	showStatus(name);	tool().activate();	}	}

		private void setSelected(ToolButton button) {	if (fSelectedToolButton != null) {	fSelectedToolButton.reset();	}	fSelectedToolButton = button;	if (fSelectedToolButton != null) {	fSelectedToolButton.select();	}	}

		public void exit() {	destroy();	dispose();	}

		protected boolean closeQuery(){	return true;	}

		protected void endApp(){	if(closeQuery() == true) {	exit();	}	}

		protected void destroy() {	}

		public void promptNew() {	newWindow(createDrawing());	}

		public void promptOpen() {	toolDone();	JFileChooser openDialog = createOpenFileChooser();	getStorageFormatManager().registerFileFilters(openDialog);	if (openDialog.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {	StorageFormat foundFormat = getStorageFormatManager().findStorageFormat(openDialog.getFileFilter());	if (foundFormat == null) {	foundFormat = getStorageFormatManager().findStorageFormat(openDialog.getSelectedFile());	}	if (foundFormat != null) {	loadDrawing(foundFormat, openDialog.getSelectedFile().getAbsolutePath());	}	else {	showStatus("Not a valid file format: " + openDialog.getFileFilter().getDescription());	}	}	}

		public void promptSaveAs() {	if (view() != null) {	toolDone();	JFileChooser saveDialog = createSaveFileChooser();	getStorageFormatManager().registerFileFilters(saveDialog);	if (saveDialog.showSaveDialog(this) == JFileChooser.APPROVE_OPTION) {	StorageFormat foundFormat = getStorageFormatManager().findStorageFormat(saveDialog.getFileFilter());	if (foundFormat == null) {	foundFormat = getStorageFormatManager().findStorageFormat(saveDialog.getSelectedFile());	}	if (foundFormat != null) {	saveDrawing(foundFormat, saveDialog.getSelectedFile().getAbsolutePath());	}	else {	showStatus("Not a valid file format: " + saveDialog.getFileFilter().getDescription());	}	}	}	}

		protected JFileChooser createOpenFileChooser() {	JFileChooser openDialog = new JFileChooser();	openDialog.setDialogType(JFileChooser.OPEN_DIALOG);	openDialog.setDialogTitle("Open File...");	return openDialog;	}

		protected JFileChooser createSaveFileChooser() {	JFileChooser saveDialog = new JFileChooser();	saveDialog.setDialogType(JFileChooser.SAVE_DIALOG);	saveDialog.setDialogTitle("Save File...");	return saveDialog;	}

		public void print() {	tool().deactivate();	PrintJob printJob = getToolkit().getPrintJob(this, "Print Drawing", null);	if (printJob != null) {	Graphics pg = printJob.getGraphics();	if (pg != null) {	((StandardDrawingView)view()).printAll(pg);	pg.dispose();	}	printJob.end();	}	tool().activate();	}

		protected void saveDrawing(StorageFormat storeFormat, String file) {	if (view() == null) {	return;	}	try {	String name = storeFormat.store(file, view().drawing());	view().drawing().setTitle(name);	setDrawingTitle(name);	}	catch (IOException e) {	showStatus(e.toString());	}	}

		protected void loadDrawing(StorageFormat restoreFormat, String file) {	try {	Drawing restoredDrawing = restoreFormat.restore(file);	if (restoredDrawing != null) {	restoredDrawing.setTitle(file);	newWindow(restoredDrawing);	}	else { showStatus("Unknown file type: could not open file '" + file + "'");	}	}	catch (IOException e) {	showStatus("Error: " + e);	}	catch (org.aspectj.lang.SoftException e) {	showStatus("Error: " + e.getWrappedThrowable());	}	}

		private void newLookAndFeel(String landf) {	try {	UIManager.setLookAndFeel(landf);	SwingUtilities.updateComponentTreeUI(this);	}	catch (Exception e) {	System.err.println(e);	}	}

		protected void setDrawingTitle(String drawingTitle) {	if (getDefaultDrawingTitle().equals(drawingTitle)) {	setTitle(getApplicationName());	}	else {	setTitle(getApplicationName() + " - " + drawingTitle);	}	}

		protected String getDrawingTitle() {	return view().drawing().getTitle();	}

		public void setApplicationName(String applicationName) {	fApplicationName = applicationName;	}

		public String getApplicationName() {	return fApplicationName;	}

		protected void setUndoManager(UndoManager newUndoManager) {	myUndoManager = newUndoManager;	}

		public UndoManager getUndoManager() {	return myUndoManager;	}

		protected VersionControlStrategy getVersionControlStrategy() {	return new StandardVersionControlStrategy(this);	}

		public String[] getRequiredVersions() {	String[] requiredVersions = new String[1];	requiredVersions[0] = VersionManagement.getPackageVersion(DrawApplication.class.getPackage());	return requiredVersions;	}

		public String getDefaultDrawingTitle() {	return fgUntitled;	}

		protected DesktopListener getDesktopListener() {	return fDesktopListener;	}

		protected void setDesktopListener(DesktopListener desktopPaneListener) {	fDesktopListener = desktopPaneListener;	}

		protected DesktopListener createDesktopListener() { return new DesktopListener() {	public void drawingViewAdded(DesktopEvent dpe) {	DrawingView dv = dpe.getDrawingView();	fireViewCreatedEvent(dv);	}	public void drawingViewRemoved(DesktopEvent dpe) {	DrawingView dv = dpe.getDrawingView();	getUndoManager().clearUndos(dv);	getUndoManager().clearRedos(dv);	fireViewDestroyingEvent(dv);	checkCommandMenus();	}	public void drawingViewSelected(DesktopEvent dpe) {	DrawingView dv = dpe.getDrawingView();	if (dv != null) {	if (dv.drawing() != null)	dv.unfreezeView();	}	setView(dv);	} };	}

		protected Iconkit createIconkit() {	return new Iconkit(this);	}

		protected void setIconkit(Iconkit newIconkit) {	fIconkit = newIconkit;	}

		protected Iconkit getIconkit() {	return fIconkit;	}


}
