/*
 * Alloy Analyzer
 * Copyright (c) 2004 Massachusetts Institute of Technology
 *
 * The Alloy Analyzer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * The Alloy Analyzer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the Alloy Analyzer; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package kodviz.gui;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

@SuppressWarnings("serial")
public class AlloyWindow extends JFrame {

	private static final long serialVersionUID = 1L;

	public AlloyWindow() {
	this(null);
    }

    public AlloyWindow(String title) {
	super(title);

	if (titleBarHeight == 0) {
	    pack();
	    titleBarHeight = getRootPane().getY();
	}

	ComponentListener cl = new ComponentAdapter() {
		public void componentShown(ComponentEvent e) {
		    addOpenWindow(AlloyWindow.this);
		}
		public void componentHidden(ComponentEvent e) {
		    removeOpenWindow(AlloyWindow.this);
		}
	    };
	addComponentListener((ComponentListener)AlloySwingUtilities.listener(cl));

	// this listener is necessary because cl.componentHidden() doesn't
	// seem to be called when a window is disposed.
	WindowListener wl = new WindowAdapter() {
		public void windowClosed(WindowEvent e) {
		    removeOpenWindow(AlloyWindow.this);
		}
	    };
	addWindowListener((WindowListener)AlloySwingUtilities.listener(wl));
    }

    public void setTitle(String title) {
	super.setTitle(title);
	refreshWindowMenus();
    }
    
    public static List openFileActionList = new LinkedList(); // of Action
    private static Set openWindowSet = new HashSet(); // of AlloyWindow
    public static Set windowMenuSet = new HashSet(); // of WindowMenu
    private static Point nextWindowLocation = new Point(0,0); // location on screen to place next new window
    private static int titleBarHeight = 0;

    @SuppressWarnings("unchecked")
    public static Set openWindows() {
	return new HashSet(openWindowSet);
    }
    
    @SuppressWarnings("unchecked")
    public static void addOpenFileAction(Action openFileAction) {
	openFileActionList.add(openFileAction);
	refreshWindowMenus();
    }

    public static void clearOpenFiles() {
	openFileActionList.clear();
	refreshWindowMenus();
    }

    @SuppressWarnings("unchecked")
    private static void addOpenWindow(AlloyWindow window) {
        openWindowSet.add(window);
        refreshWindowMenus();
    }

    private static void removeOpenWindow(AlloyWindow window) {
        openWindowSet.remove(window);
        refreshWindowMenus();
    }

    public void positionNicely() {
        setLocation(nextWindowLocation);

        nextWindowLocation =
            new Point(nextWindowLocation.x + titleBarHeight, nextWindowLocation.y + titleBarHeight);
        if (nextWindowLocation.y > (AlloySwingUtilities.getScreenHeight() / 2))
            nextWindowLocation = new Point(0, 0);
    }

    public static void refreshWindowMenus() {
        Iterator menus = windowMenuSet.iterator();
        while (menus.hasNext()) {
            WindowMenu windowMenu = (WindowMenu)menus.next();
            windowMenu.refresh();
        }
    }

    public static JMenu makeWindowMenu(String name) {
	return new WindowMenu(name);
    }

    private static class WindowMenu extends JMenu {
 		private static final long serialVersionUID = 1L;
	@SuppressWarnings("unchecked")
	WindowMenu(String name) {
	    super(name);
	    refresh();
	    windowMenuSet.add(WindowMenu.this);
	}
	
	void refresh() {
	    removeAll();
	    
	    Iterator i = openWindowSet.iterator();
	    while (i.hasNext()) {
		final AlloyWindow window = (AlloyWindow)i.next();
		String title = window.getTitle();
		JMenuItem item = new JMenuItem();
		item.setAction(AlloySwingUtilities.action(new AbstractAction((title == null || title.equals("")) ? "- no title -" : title) {
			public void actionPerformed(ActionEvent e) {
			    window.setVisible(true);
			    window.toFront();
			    window.requestFocus();
			}
		    }));
		add(item);
	    }

	    // add the list of open files
	    if (!openFileActionList.isEmpty()) {
		addSeparator();
	    }
	    for (Iterator j = openFileActionList.iterator(); j.hasNext();) {
		Action openFileAction = (Action)j.next();
		JMenuItem item = new JMenuItem();
		item.setAction(openFileAction);
		add(item);
	    }

	}
    }
}
