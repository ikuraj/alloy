package kodviz.gui;

import java.awt.Point;
import java.io.File;
import java.util.HashSet;
import java.util.Set;

import kodviz.util.CleanupManager;

@SuppressWarnings("unchecked")
public class KodVizGUIFactory {
	private static Set GUIs = new HashSet();
	private static Point nextWindowLocation = new Point(0,0);
	
	/**
	 * Creates a new visualizer window. If the file cannot be loaded, no
	 * window will be created.
	 * @param f the file that contains the Alloy solution to be loaded
	 */
	public void create(File f) {
		try {
			KodVizGUI ret = new KodVizGUI(f, this);
			GUIs.add(ret);
		} catch (KodVizGUIException e) {}
	}
	
	/**
	 * Returns the number of visualizer windows belonging to this process
	 * @return the number of existing visuazlier windows
	 */
	public int size() {
		return GUIs.size();
	}
	
	/**
	 * Places the given <code>KodVizGUI</code> instance nicely. The position
	 * is then updated so that the next call to this method will place the
	 * next window in a cascade.
	 * @param k the <code>KodVizGUI</code> instance to be placed nicely
	 */
    public void positionNicely(KodVizGUI k) {
    	k.setLocation(nextWindowLocation);
    	int titleBarHeight = k.getRootPane().getY();
        nextWindowLocation =
            new Point(nextWindowLocation.x + titleBarHeight,
            		nextWindowLocation.y + titleBarHeight);
        if (nextWindowLocation.y > (AlloySwingUtilities.getScreenHeight() / 2))
            nextWindowLocation = new Point(0, 0);
    }

    /**
     * Disposes of the specified visualizer window. Note that the visualizer
     * window will be removed without warning.
     * @param k the visualizer window to be removed
     */
	public void kill(KodVizGUI k) {
		if (GUIs.contains(k)) {
			GUIs.remove(k);
			k.dispose();
			if (GUIs.size() == 0)
				System.exit(0);
		}
	}
	
	/**
	 * Removes all visualizer windows spawned by this factory, and terminates
	 * the program.
	 */
	public void killAll() {
		while (!GUIs.isEmpty()) {
			KodVizGUI next = (KodVizGUI)(GUIs.iterator().next());
			next.closeThisVisualizer();
		}
		exit();
	}
	
	/**
	 * Quit.
	 */
	private void exit() {
		CleanupManager.exit(0);
	}
	
	/**
	 * Loads an empty visualizer window. If <code>args</code>
	 * contain elements, load them as file names.
	 * @param args the array of file names to be loaded. They willbe
	 *  loaded into separate visualizer windows
	 */
	public static void main(String[] args) {
		// if (args.length != 1) System.exit(-1);
		KodVizGUIFactory factory = new KodVizGUIFactory();
		File f = null;
		for (int i = 0; i < args.length; i++) {
			f = new File(args[i]);
			factory.create(f);
		}
		// start a default window
		if (factory.size() == 0)
			factory.create(null);
	}
}
