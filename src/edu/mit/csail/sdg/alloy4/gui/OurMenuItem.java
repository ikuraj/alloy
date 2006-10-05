package edu.mit.csail.sdg.alloy4.gui;

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

public final class OurMenuItem extends JMenuItem {
	
	private static final long serialVersionUID = 1L;

	public final OurMenu parent;

	public OurMenuItem(OurMenu in_parent, String label, int key, int accel, final String message) {
		super(label,key);
		parent=in_parent;
        int accelMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
        if (accel>=0) setAccelerator(KeyStroke.getKeyStroke(accel, accelMask));
        if (message!=null) addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { parent.handleMessage(OurMenuItem.this, message); }
        });
	}
}
