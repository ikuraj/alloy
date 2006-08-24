package kodviz.gui;

import java.awt.BorderLayout;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;

import ext.nanoxml.XMLParseException;

import kodviz.alloyviz.IncompatiblePaletteException;
import kodviz.alloyviz.VizInstance;
import kodviz.util.ExtensionFileFilter;
import kodviz.util.FileChooser;
import kodviz.util.Params;
import kodviz.util.TmpFiles;
import kodviz.xml.AlloyXMLParser;
import kodviz.xml.PaletteXMLParseException;

/**
 * KodVizGUI represents a single visualizer window that functions independently
 * of others. It is managed by <code>KodVizGUIFactory</code>.
 * 
 * @author jbaek
 */

@SuppressWarnings("serial")
public class KodVizGUI extends JFrame {

	private static final long serialVersionUID = 1L;

	// factory
	private final KodVizGUIFactory parent;
	
	// Alloy solution instance
	private VizInstance _vizInstance;
	private VizPanel _vizPanel;

	// GUI elements
	private JSplitPane _horizontalSplitPane;
	private JToolBar _viewsToolBar;
	private JMenu _viewsMenu;
	private int _lastSplitPaneDivider = 500;
	
	// visibility settings
	private boolean _settingsOpen = false;
	private boolean _currentlySettingsOpen = false;
	private int _currentDisplaySelection = VIZ_MODE;

	// file i/o
	private File latestFile = null;
	private boolean modelLoaded = false;
	
	private static final String PALETTE_EXTENSION = "pal";
	private static final String XML_EXTENSION = "xml";
	private final ExtensionFileFilter extFileFilter;
	private final ExtensionFileFilter xmlFileFilter;
	
	private final FileChooser chooser;

	// buttons and associated constants
	JButton openSettingsButton;
	JButton closeSettingsButton;
	JButton updateSettingsButton;
	private JButton vizButton = makeAlloySolutionButton("Viz",
			"Show Visualization", "images/24_graph.gif", VIZ_MODE);

	private static final int VIZ_MODE = 0;
	private static final int DOT_MODE = 1;
	private JButton[] solutionButtons = {
			vizButton,
			makeAlloySolutionButton("Dot", "Show Dot",
					"images/24_plaintext.gif", DOT_MODE), };

	/**
	 * Creates a new instance of <code>KodVizGUI</code> with the given
	 * file <code>f</cdoe> as the Alloy XML source and <code>p</code>
	 * as the parent factory. 
	 * @param f the file that contains the Alloy solution data in XML
	 * @param p the parent factory instance
	 * @throws KodVizGUIException if <code>KodVizGUI</code> fails to load
	 *  the given file
	 */
	public KodVizGUI(File f, KodVizGUIFactory p) throws KodVizGUIException {
		super("Alloy Visualizer");
		parent = p;
		setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		addWindowListener((WindowListener) AlloySwingUtilities.listener(new WindowAdapter() {
					public void windowClosing(WindowEvent e) {
						closeThisVisualizer();
					}
				}));

		// set up file chooser
		chooser = new FileChooser(Params.glob.getParam("MAIN", "modeldir"));
		extFileFilter = new ExtensionFileFilter(PALETTE_EXTENSION);
		xmlFileFilter = new ExtensionFileFilter(XML_EXTENSION);

		// create menubar/toolbar
		_viewsMenu = createMenuBar();
		_viewsToolBar = createViewsToolBar();
		_viewsToolBar.setEnabled(false);

		// create the horizontal split
		_horizontalSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		_horizontalSplitPane.setResizeWeight(0.5);
		_horizontalSplitPane.setContinuousLayout(true);
		_horizontalSplitPane.setBorder(null);

		// initialize model
		instantiate(f, true);
		// pack, etc
		pack();
		setSize(700, 700);
		parent.positionNicely(this);
		setVisible(true);
	}

	/**
	 * Enables the View menu and the toolbar. This should be called
	 * once a valid Alloy solution instance is loaded.
	 */
	private void enableModelFeatures() {
		_viewsToolBar.setEnabled(true);
		_viewsMenu.setEnabled(true);
	}
	
	/**
	 * Instantiates the current <code>KodVizGUI</code> instance using
	 * the given file <code>f</code>.
	 * @param f the file that contains the Alloy solution data in XML
	 * @param quitOnFailure if true, <code>KodVizGUIException</code> will be
	 *  thrown if the file cannot be loaded
	 * @throws KodVizGUIException see <code>quitOnFailure</code>
	 */
	private void instantiate(File f, boolean quitOnFailure)
	throws KodVizGUIException {
		if (f == null) return;
		_vizInstance = loadFile(f);
		if (_vizInstance == null) { // failed to load file
			_vizPanel = null;
			JOptionPane.showMessageDialog(null,
					"File is not of valid Alloy XML format.",
					"Error",
					JOptionPane.ERROR_MESSAGE);
			if (quitOnFailure) {
				throw new KodVizGUIException();
			}
		} else {
			_vizPanel = new VizPanel(_vizInstance, null, false);
			modelLoaded = true;
			this.setTitle("Alloy Visualizer: " + f.getName());
			enableModelFeatures();
			updateDisplay();
		}
	}
	
	/**
	 * Loads an Alloy solution instance from the given file.
	 * @param f the file that contains the Alloy solution XML
	 * @return a <code>VizInstance</code> structure that represents
	 *  the Alloy solution; null if the file cannot be loaded
	 */
	private static VizInstance loadFile(File f) {
		VizInstance _instance = AlloyXMLParser.readXMLfile(f);
		return _instance;
	}

	/**
	 * Grabs and returns the panel corresponding to the current visualization
	 * mode. This panel is shown on the right half of the application. This
	 * method should be only called once an Alloy solution has successfully
	 * been instantiated.
	 * @return the panel that contains the current visualization
	 */
	private JComponent getChosenPanel() {
		// by invoking getCurrentGraph, we update the dotfile as well.
		JComponent tmp = _vizPanel.getCurrentGraph();
		switch (_currentDisplaySelection) {
		case VIZ_MODE:
			return tmp;
		case DOT_MODE:
			return getDotTextComponent();
		default:
			return tmp; // fall back to visualizer
		}
	}

	/**
	 * Updates the display of the right-side panel with the given mode
	 * @param MODE specifies the desired panel
	 */
	public void updateDisplay(int MODE) {
		if (MODE == VIZ_MODE || MODE == DOT_MODE)
			_currentDisplaySelection = MODE;
		updateDisplay();
	}

	/**
	 * Refreshes the right-side visualization panel with the latest data
	 */
	public void updateDisplay() {
		final Box instanceTopBox = Box.createHorizontalBox();
		final JPanel instanceArea = new JPanel(new BorderLayout());
		instanceTopBox.add(_viewsToolBar);
		instanceArea.add(instanceTopBox, BorderLayout.NORTH);
		instanceArea.add(getChosenPanel(), BorderLayout.CENTER);
		instanceArea.setVisible(true);

		if (_settingsOpen) {
			if (_currentlySettingsOpen)
				_lastSplitPaneDivider = _horizontalSplitPane
						.getDividerLocation();
			_horizontalSplitPane.setRightComponent(instanceArea);
			_horizontalSplitPane.setLeftComponent(
					_vizPanel.getSettingsPanel());
			_horizontalSplitPane.setDividerLocation(_lastSplitPaneDivider);
			setContentPane(_horizontalSplitPane);
		} else {
			if (_currentlySettingsOpen)
				_lastSplitPaneDivider = _horizontalSplitPane
						.getDividerLocation();
			setContentPane(instanceArea);
		}
		validate();
		_currentlySettingsOpen = _settingsOpen;
	}

	/**
	 * Creates the menu for visualizer, sets it as the current menubar,
	 * and returns the view menu of it. 
	 */
	private JMenu createMenuBar() {
        final int shortcutKeyMask = AlloySwingUtilities.getShortcutKeyMask();
		JMenuBar mb = new JMenuBar();
		JMenu fileMenu = new JMenu("File");
		fileMenu.setMnemonic('F');
		mb.add(fileMenu);
		JMenu viewMenu = new JMenu("View");
		viewMenu.setMnemonic('V');
		mb.add(viewMenu);

		final JMenuItem openItem = new JMenuItem("Open...");
		openItem.setAction(AlloySwingUtilities.action(new AbstractAction(
				"Open...") {
			public void actionPerformed(final ActionEvent e) {
				openNewInstance();
			}
		}));
        openItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O,
        		shortcutKeyMask));
		fileMenu.add(openItem);

        JMenuItem closeMenuItem = new JMenuItem("Close", 'C');
        closeMenuItem.setAction(AlloySwingUtilities.action(new AbstractAction(
        		"Close") {
        	public void actionPerformed(final ActionEvent e) {
        		closeThisVisualizer();
        	}
	    }));
        closeMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_W,
        		shortcutKeyMask));
        fileMenu.add(closeMenuItem);
        fileMenu.addSeparator();
        
        JMenuItem quitMenuItem = new JMenuItem("Quit", 'Q');
        quitMenuItem.setAction(AlloySwingUtilities.action(new AbstractAction(
        		"Quit") {
        	public void actionPerformed(final ActionEvent e) {
        		closeAllVisualizers();
        	}
	    }));
        quitMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q,
        		shortcutKeyMask));
        fileMenu.add(quitMenuItem);
        
		final JMenuItem loadViewItem = new JMenuItem("Load View...");
		loadViewItem.setAction(AlloySwingUtilities.action(new AbstractAction(
				"Load View...") {
			public void actionPerformed(final ActionEvent e) {
				loadPalette();
			}
		}));
		viewMenu.add(loadViewItem);

		final JMenuItem saveViewItem = new JMenuItem("Save View");
		saveViewItem.setAction(AlloySwingUtilities.action(new AbstractAction(
				"Save View") {
			public void actionPerformed(final ActionEvent e) {
				if (latestFile == null)
					savePalette();
				else
					savePalette(latestFile);
			}
		}));
		viewMenu.add(saveViewItem);

		final JMenuItem saveViewAsItem = new JMenuItem("Save View As...");
		saveViewAsItem.setAction(AlloySwingUtilities.action(new AbstractAction(
				"Save View As...") {
			public void actionPerformed(final ActionEvent e) {
				savePalette();
			}
		}));
		viewMenu.add(saveViewAsItem);
		viewMenu.addSeparator();
		
		final JMenuItem clearViewItem = new JMenuItem("Clear View");
		clearViewItem.setAction(AlloySwingUtilities.action(new AbstractAction(
				"Clear View") {
			public void actionPerformed(final ActionEvent e) {
				clearPalette();
			}
		}));
		viewMenu.add(clearViewItem);
		
		viewMenu.setEnabled(false);
		this.setJMenuBar(mb);
		return viewMenu;
	}

	/**
	 * Creates the toolbar on top of the visualizer panel
	 * @return a toolbar that contains visualization buttons
	 */
	private JToolBar createViewsToolBar() {
		JToolBar toolbar = new JToolBar();

		openSettingsButton = new AlloyButton();
		openSettingsButton.setAction(AlloySwingUtilities
				.action(new AbstractAction("Layout", new ImageIcon(
						loadImage("images/24_settings.gif"))) {
					public void actionPerformed(final ActionEvent e) {
						_settingsOpen = true;
						updateViewModeButtons();
					}
				}));
		openSettingsButton.setToolTipText("Open Layout Settings");
		toolbar.add(openSettingsButton);

		closeSettingsButton = new AlloyButton(AlloySwingUtilities
				.action(new AbstractAction("Close Layout", new ImageIcon(
						loadImage("images/24_settings_close2.gif"))) {
					public void actionPerformed(final ActionEvent e) {
						_settingsOpen = false;
						updateViewModeButtons();
					}
				}));
		closeSettingsButton.setToolTipText("Close Layout Settings");
		closeSettingsButton.setVisible(false);
		toolbar.add(closeSettingsButton);

		updateSettingsButton = new AlloyButton(AlloySwingUtilities
				.action(new AbstractAction("Update", new ImageIcon(
						loadImage("images/24_settings_apply2.gif"))) {
					public void actionPerformed(final ActionEvent e) {
						updateDisplay();
					}
				}));
		updateSettingsButton.setToolTipText("Update");
		toolbar.add(updateSettingsButton);
		for (int i = 0; i < solutionButtons.length; i++) {
			toolbar.add(solutionButtons[i]);
		}

		toolbar.setFloatable(false);
		updateViewModeButtons();

		return toolbar;
	}

	/**
	 * Changes the visibility of the open/close-settings button to reflect
	 * the current status.
	 */
	private void updateViewModeButtons() {
		closeSettingsButton.setVisible(_settingsOpen);
		openSettingsButton.setVisible(!_settingsOpen);
		if (_settingsOpen != _currentlySettingsOpen)
			updateDisplay();
	}

	/**
	 * A wrapper for loading an image from the local storage.
	 * @param pathName the path name for the image file
	 * @return an image loaded from the given file
	 */
	private static Image loadImage(final String pathName) {
		return AlloySwingUtilities.loadImage(pathName);
	}

	// taken from the class SuccessfulAnalysis and simplified
	/**
	 * Retrieves the Dot text output of the current instance. This should only
	 * be called once an Alloy solution has been successfully instantiated.
	 * This method regenerates the return value every time it is called.
	 */
	private JComponent getDotTextComponent() {
		// if (_dotComponent != null) return _dotComponent;
		final File dotFile = TmpFiles.createHandleToATempFile("dotfile.dot");
		String text;
		final String NL = System.getProperty("line.separator");
		if (dotFile.canRead()) {
			try {
				final StringBuffer b = new StringBuffer(
						(int) dotFile.length() + 10);
				b.append("# ");
				b.append(dotFile.getAbsolutePath());
				b.append(NL);
				final BufferedReader r = new BufferedReader(new FileReader(
						dotFile));
				String line;
				while ((line = r.readLine()) != null) {
					b.append(line);
					b.append(NL);
				}
				text = b.toString();
			} catch (Exception e) {
				text = "Exception reading " + dotFile.getAbsolutePath() + NL
						+ e;
			}
		} else {
			text = dotFile.getAbsolutePath() + " cannot be read";
		}
		return makeTextArea(text);
	}

	// taken from class SuccessfulAnalysis and simplified
	/**
	 * Returns a <code>JScrollPane</code> that contains a textbox
	 * with the supplied argument as its value.
	 * @param text the text to appear in the textbox
	 */
	private JScrollPane makeTextArea(final String text) {
		final JTextArea t = new JTextArea(text);
		t.setEditable(false);
		return new JScrollPane(t);
	}

	// taken from the full GUI
	/**
	 * Creates and returns a button to be used in the toolbar.
	 */
	private AlloySolutionButton makeAlloySolutionButton(final String label,
			final String toolTip, final String imageName, final int MODE) {
		final AlloySolutionButton button = new AlloySolutionButton(label,
				imageName, MODE);
		button.setToolTipText(toolTip);
		return button;
	}

	/**
	 * Borrowed from class NewAlloyGUI in Alloy 3.
	 */
	private class AlloySolutionButton extends AlloyButton {
		private static final long serialVersionUID = 1L;
		public AlloySolutionButton(final String label, final String imageName,
				final int MODE) {
			setAction(AlloySwingUtilities.action(new AbstractAction(label,
					new ImageIcon(loadImage(imageName))) {
				public void actionPerformed(ActionEvent e) {
					updateDisplay(MODE);
				}
			}));
		} // end constructor
	} // end class AlloySolutionButton

	/**
	 * Clears the current view and resets it.
	 */
	private void clearPalette() {
		chooser.setFileFilter(extFileFilter);
		if (_vizPanel.isPaletteChanged()) {
			int opt = JOptionPane.showConfirmDialog(null, "Palette has not "
					+ "been saved. Do you want to save the current palette"
							+ ((latestFile != null) ? (" as " + latestFile
									.getName()) : "") + "?", "Warning",
					JOptionPane.YES_NO_CANCEL_OPTION,
					JOptionPane.WARNING_MESSAGE);
			if (opt == JOptionPane.CANCEL_OPTION)
				return; // do nothing
			if (opt == JOptionPane.YES_OPTION) {
				if (latestFile == null) {
					savePalette();
				} else
					savePalette(latestFile);
			} else if (opt == JOptionPane.NO_OPTION) {
				loadPalette(null);
			}
		} else /* !_vizState.paletteChanged() */ {
			loadPalette(null);
		}
	}
	
	/**
	 * Loads a palette XML file from the specified file into the current
	 * instance of <code>KodVizGUI</code> and updates the visualization.
	 * Displays an error message if the error is caught. This should only
	 * be called once an Alloy solution has been successfully instantiated.
	 * @param f the palette XML file to load from; if null, an empty palette
	 *  will be loaded
	 * 
	 */
	private void loadPalette(File f) {
		try {
			_vizPanel.loadPaletteXML(f);
		} catch (IncompatiblePaletteException ipe) {
			JOptionPane.showMessageDialog(null,
					"File uses an incompatible palette version.",
					"Version error", JOptionPane.ERROR_MESSAGE);
			return;
		} catch (XMLParseException xe) { // jbaek added
			JOptionPane.showMessageDialog(null, "Invalid XML Format.",
					"Parse Error", JOptionPane.ERROR_MESSAGE);
		} catch (PaletteXMLParseException xe) { // jbaek added
			JOptionPane.showMessageDialog(null,
					"Error parsing the palette XML.", "Parse Error",
					JOptionPane.ERROR_MESSAGE);
		} catch (Exception er) {
			JOptionPane.showMessageDialog(null, "Error loading palette."
					+ er.toString(), "Error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		latestFile = f;
		updateDisplay();

	}

	/**
	 * Load a palette from a file designated in a dialog box.
	 */
	private void loadPalette() {
		chooser.setFileFilter(extFileFilter);
		if (_vizPanel.isPaletteChanged()) {
			int opt = JOptionPane.showConfirmDialog(null, "Palette has not "
					+ "been saved. Do you want to save the current palette"
							+ ((latestFile != null) ? (" as " + latestFile
									.getName()) : "") + "?", "Warning",
					JOptionPane.YES_NO_CANCEL_OPTION,
					JOptionPane.WARNING_MESSAGE);
			if (opt == JOptionPane.CANCEL_OPTION)
				return; // do nothing
			if (opt == JOptionPane.YES_OPTION) {
				if (latestFile == null) {
					savePalette();
				} else
					savePalette(latestFile);
			} else if (opt == JOptionPane.NO_OPTION) {
				int returnVal = chooser.showOpenDialog(null);
				if (returnVal == FileChooser.APPROVE_OPTION) {
					File file = chooser.getSelectedFile();
					loadPalette(file); // actual method to load file
				}
			}
		} else /* !_vizState.paletteChanged() */{
			int returnVal = chooser.showOpenDialog(null);
			if (returnVal == FileChooser.APPROVE_OPTION) {
				File file = chooser.getSelectedFile();
				loadPalette(file);
			}
		}
	}

	/**
	 * Saves the current palette into a user-specified file from a dialog box.
	 */
	private void savePalette() {
		chooser.setFileFilter(extFileFilter);
		int returnVal = chooser.showSaveDialog(null);
		if (returnVal == FileChooser.APPROVE_OPTION) {
			File file = extFileFilter.addExtensionIfNone(chooser
					.getSelectedFile());
			if (file.exists()) {
				int opt = JOptionPane.showConfirmDialog(null,
					"File exists.  Merge/Overwrite data?", "Warning",
					JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
				if (opt != JOptionPane.NO_OPTION) {
					savePalette(file);
				}
			} else
				savePalette(file);
		}
	}

	/**
	 * Loads a palette XML file from the specified file.
	 * @param f the XML file to load a palette from
	 */
	private void savePalette(File f) {
		chooser.setFileFilter(extFileFilter);
		if (f == null) {
			savePalette();
			return;
		}
		try {
			_vizPanel.savePaletteXML(f);
		} catch (PaletteXMLParseException xe) { // jbaek added
			JOptionPane.showMessageDialog(null,
					"Unresolved parsing exception.", "Parse Error",
					JOptionPane.ERROR_MESSAGE);
		} catch (IOException ie) { // jbaek added
			JOptionPane.showMessageDialog(null,
					"Unable to write to the specified file.", "I/O Error",
					JOptionPane.ERROR_MESSAGE);
		} catch (Exception er) {
			JOptionPane.showMessageDialog(null, "Error saving palette."
					+ er.toString(), "Error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		latestFile = f;
		_vizPanel.setPaletteAsUnchanged();
	}

	/**
	 * Opens a new instance of <code>KodVizGUI</code> outside this current
	 * window, using a file specified in a dialog box. If the file cannot
	 * be loaded successfully, the newly created visualizer instance will
	 * be disposed of. If there is no existing solution loaded to the current
	 * visualizer instance, the file will be loaded into it instead of creating
	 * a new window.
	 */
	private void openNewInstance() {
		chooser.setFileFilter(xmlFileFilter);
		int returnVal = chooser.showOpenDialog(null);
		if (returnVal == FileChooser.APPROVE_OPTION) {
			File file = chooser.getSelectedFile();
			if (!modelLoaded) { // instantiate the current window
				try {
					instantiate(file, false);
				} catch (KodVizGUIException e) {}
			} else
				parent.create(file);
		}
	}
	
	/**
	 * Closes the current instance of <code>KodVizGUI</code>.
	 */
	protected void closeThisVisualizer() {
		remove(getJMenuBar());
		this.setVisible(false);
		parent.kill(this);
	}
	
	/**
	 * Closes all instances of <code>KodVizGUI</code> belonging to the
	 * current process.
	 */
	protected void closeAllVisualizers() {
		parent.killAll();
	}
}
