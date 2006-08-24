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

import java.awt.BorderLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSplitPane;

@SuppressWarnings("serial")
class AlloyPanel extends JPanel {

	private static final long serialVersionUID = 1L;

	private JFrame theWindow;

    private JPanel mainPanel;
    private JPanel centralPanel;
    private JSplitPane splitPane;

    private Box topBox;
    private Box customTopBox;
    private JPanel contentPanel;
    private boolean isAttached;
    private boolean isShown;
    private boolean windowNeverBeenOpened;

    private JLabel viewerLabel;
    private JButton tearOffButton;
    private JButton detachButton;
    private JButton attachButton;
    private JButton hideButton;

    private Set alloyPanelListeners;

    AlloyPanel(JComponent content,
	       String title) {
        this();
        getContentPanel().add(content);
        setTitle(title);
    }

    AlloyPanel() {
        mainPanel = new JPanel(new BorderLayout());
        centralPanel = new JPanel(new BorderLayout());
        contentPanel = new JPanel(new BorderLayout());
        setLayout(new BorderLayout());
        windowNeverBeenOpened = true;
        isShown = true;
        alloyPanelListeners = new HashSet();

        theWindow = new AlloyWindow();
        theWindow.getContentPane().setLayout(new BorderLayout());
        theWindow.setVisible(false);
        theWindow.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
	theWindow.addWindowListener((WindowListener)AlloySwingUtilities
				    .listener(new WindowAdapter() {
					    public void windowClosing(WindowEvent e) {
						if (hideButton.isEnabled())
						    makeHidden();
					    }
					}));

	topBox = Box.createHorizontalBox();

        //setBackground(Color.white);
        topBox.setBorder(AlloySwingUtilities.createBottomBorder());

	viewerLabel = new JLabel();
	topBox.add(viewerLabel);

        customTopBox = Box.createHorizontalBox();
        topBox.add(customTopBox);

        topBox.add(Box.createHorizontalGlue());

        if (tearOffAction() != null) {
	    tearOffButton = new JButton("");
	    tearOffButton.setAction(AlloySwingUtilities.action(tearOffAction()));
	    tearOffButton.setIcon(new ImageIcon(AlloySwingUtilities.loadImage("images/12_tearoff.gif")));
	    tearOffButton.setMargin(new Insets(0, 0, 0, 0));
	    tearOffButton.setBorderPainted(false);
	    tearOffButton.setToolTipText("Tear-off Copy");
	    topBox.add(tearOffButton);
        }

	detachButton = new JButton();
	detachButton
	    .setAction(
		       AlloySwingUtilities
		       .action(new AbstractAction(
						  "",
						  new ImageIcon(AlloySwingUtilities.loadImage("images/12_detach.gif"))) {
			       public void actionPerformed(ActionEvent e) {
				   detach();
			       }
			   }));
	detachButton.setMargin(new Insets(0, 0, 0, 0));
        detachButton.setBorderPainted(false);
        detachButton.setToolTipText("Detach");
	topBox.add(detachButton);

	attachButton = new JButton("");
	attachButton
	    .setAction(
		       AlloySwingUtilities
		       .action(new AbstractAction(
						  "",
						  new ImageIcon(AlloySwingUtilities.loadImage("images/12_attach.gif"))) {
			       public void actionPerformed(ActionEvent e) {
				   attach();
			       }
			   }));
	attachButton.setMargin(new Insets(0, 0, 0, 0));
        attachButton.setBorderPainted(false);
        attachButton.setToolTipText("Attach");
        topBox.add(attachButton);
        attachButton.setVisible(false);

	hideButton = new JButton("");
	hideButton
	    .setAction(
		       AlloySwingUtilities
		       .action(new AbstractAction("", new ImageIcon(AlloySwingUtilities.loadImage("images/12_close.gif"))) {
			       public void actionPerformed(ActionEvent e) {
				   makeHidden();
			       }
			   }));
	hideButton.setMargin(new Insets(0, 0, 0, 0));
        hideButton.setBorderPainted(false);
        hideButton.setToolTipText("Hide");
	topBox.add(hideButton);

        centralPanel.add(BorderLayout.CENTER, contentPanel);
        centralPanel.add(BorderLayout.NORTH, topBox);
        mainPanel.add(centralPanel);
        add(mainPanel);
        isAttached = true;
    }

    void detach() {
        Point panelLocation = mainPanel.getLocationOnScreen();
        hidePanel();
        remove(mainPanel);
        viewerLabel.setVisible(false);
        detachButton.setVisible(false);
        attachButton.setVisible(true);
        theWindow.getContentPane().add(mainPanel);
        isAttached = false;
        if (windowNeverBeenOpened) {
	    mainPanel.setPreferredSize(mainPanel.getSize());
	    theWindow.pack();
	    mainPanel.setPreferredSize(null);
	    theWindow.setLocation(panelLocation);
	    windowNeverBeenOpened = false;
        }
        showWindow();
        fireAlloyPanelDetached();
    }

    void attach() {
        hideWindow();
        theWindow.getContentPane().remove(mainPanel);
        viewerLabel.setVisible(true);
        attachButton.setVisible(false);
        detachButton.setVisible(true);
        add(mainPanel);
        isAttached = true;
        showPanel();
        fireAlloyPanelAttached();
    }

    void makeShown() {
        if (isAttached)
	    showPanel();
        else
	    showWindow();
        isShown = true;
        fireAlloyPanelShown();
    }

    void makeHidden() {
        if (isAttached)
	    hidePanel();
        else
	    hideWindow();
        isShown = false;
        fireAlloyPanelHidden();
    }

    private void hideWindow() {
        theWindow.setVisible(false);
    }

    private void hidePanel() {
        setVisible(false);
    }

    private void showWindow() {
        theWindow.setVisible(true);
    }

    private void showPanel() {
        setVisible(true);
    }

    @SuppressWarnings("unchecked")
    public void addAlloyPanelListener(AlloyPanelListener l) {
        alloyPanelListeners.add(l);
    }

    public void removeAlloyPanelListener(AlloyPanelListener l) {
        alloyPanelListeners.remove(l);
    }

    private void fireAlloyPanelHidden() {
        Iterator it = alloyPanelListeners.iterator();
        while (it.hasNext()) {
	    AlloyPanelListener l = (AlloyPanelListener)it.next();
	    l.alloyPanelHidden(new AlloyPanelEvent(AlloyPanel.this));
        }
    }

    private void fireAlloyPanelShown() {
        Iterator it = alloyPanelListeners.iterator();
        while (it.hasNext()) {
	    AlloyPanelListener l = (AlloyPanelListener)it.next();
	    l.alloyPanelShown(new AlloyPanelEvent(AlloyPanel.this));
        }
    }

    private void fireAlloyPanelAttached() {
        Iterator it = alloyPanelListeners.iterator();
        while (it.hasNext()) {
	    AlloyPanelListener l = (AlloyPanelListener)it.next();
	    l.alloyPanelAttached(new AlloyPanelEvent(AlloyPanel.this));
        }
    }

    private void fireAlloyPanelDetached() {
        Iterator it = alloyPanelListeners.iterator();
        while (it.hasNext()) {
	    AlloyPanelListener l = (AlloyPanelListener)it.next();
	    l.alloyPanelDetached(new AlloyPanelEvent(AlloyPanel.this));
        }
    }


    JPanel getContentPanel() {
        return contentPanel;
    }

    Box getCustomTopBox() {
        return customTopBox;
    }

    void setTitle(String title) {
        theWindow.setTitle(title);
        viewerLabel.setText(title+" ");
    }

    void showLeftPanel(JPanel leftPanel) {
        mainPanel.remove(centralPanel);
        splitPane = new AlloySplitPane(JSplitPane.HORIZONTAL_SPLIT,
				       leftPanel,
				       centralPanel);
        mainPanel.add(splitPane);
    }

    void hideLeftPanel() {
        mainPanel.remove(splitPane);
        splitPane = null;
        mainPanel.add(centralPanel);
    }

    JFrame getWindow() {
        return theWindow;
    }

    void setTearOffEnabled(boolean b) {
        tearOffButton.setEnabled(b);
    }

    void setAttachDetachEnabled(boolean b) {
        attachButton.setEnabled(b);
        detachButton.setEnabled(b);
    }

    void setHideEnabled(boolean b) {
        hideButton.setEnabled(b);
    }

    boolean isAttached() {
        return isAttached;
    }

    boolean isShown() {
        return isShown;
    }

    // to enable the tear-off button, override this method
    // to return the action that should occur each time the
    // tear-off butotn is pressed
    Action tearOffAction() { return null; }
}
