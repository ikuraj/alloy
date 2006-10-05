package edu.mit.csail.sdg.alloy4.gui;

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;
import edu.mit.csail.sdg.alloy4.util.MessageHandler;

public class OurMenu extends JMenu {

	private static final long serialVersionUID = 1L;
	
	private final MessageHandler handler;
	
	public OurMenu(MessageHandler handler, String label, boolean tearoff) {
		super(label,tearoff);
		this.handler=handler;
	}
	
    public JMenuItem addMenuItem(String label, int key, int accel, final String message) {
        JMenuItem ans = new JMenuItem(label,key);
        int accelMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
        if (accel>=0) ans.setAccelerator(KeyStroke.getKeyStroke(accel, accelMask));
        if (handler!=null) ans.addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { handler.handleMessage(message); }
        });
        this.add(ans);
        return ans;
    }
}
