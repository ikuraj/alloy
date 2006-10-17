package edu.mit.csail.sdg.alloy4;

import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * This implements a graphical on/off checkbox.
 *
 * <p/> To construct a OurBinaryCheckbox object, you need to give it a GetterSetter object and a key.
 * <br/> Whenever the checkbox needs to be repainted, it calls gs.get(key) to find out whether it is ON or OFF.
 * <br/> Whenever the checkbox is clicked by the user, it calls gs.set(key,value) to set a new value.
 *
 * <p/> In other words, the checkbox does not contain any hidden states: all changes are immediately
 * committed into the data store, and it always re-queries the data store whenever it needs to know if it's on or off.
 *
 * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
 *
 * @author Felix Chang
 */

public final class OurBinaryCheckbox extends JPanel {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1;

    /** The icon to use when the checkbox is off. */
    private static final ImageIcon off = OurUtil.loadIcon("images/cb0.gif");

    /** The icon to use when the checkbox is on. */
    private static final ImageIcon on  = OurUtil.loadIcon("images/cb1.gif");

    /** The underlying JCheckBox object. */
    private final JCheckBox box;

    /** The JLabel object for displaying a label next to the checkbox. */
    private final JLabel jlabel;

    /** The identifier associated with this checkbox. */
    private final Object key;

    /**
     * The GetterSetter associated with this checkbox.
     * <p/> When the checkbox needs to be repainted, we call gs.get(key) to find out whether it is ON or OFF.
     * <p/> When the checkbox is clicked by the user, we call gs.set(key,value) to set a new value.
     */
    private final GetterSetter gs;

    /**
     * This interface defines the "boolean get(Object key)" and "set(Object key, boolean value)" methods.
     *
     * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
     *
     * @author Felix Chang
     */
    public interface GetterSetter {
        /** This method reads the boolean value associated with the key; this must be called only by the AWT thread. */
        public boolean get(Object key);
        /** This method sets the boolean value associated with the key; this must be called only by the AWT thread. */
        public void set(Object key, boolean value);
    }

    /**
     * Constructs a new binary checkbox.
     * @param gs - the GetterSetter associated with this checkbox
     * @param key - the key associated with this checkbox
     * @param label - the label to display next to the checkbox
     * @param tip - the tool tip to show when the mouse hovers over this check box
     */
    public OurBinaryCheckbox(final GetterSetter gs, final Object key, String label, String tip) {
        super();
        this.gs=gs;
        this.key=key;
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        box = new JCheckBox(off); // Doesn't matter if it's on or off; the paint() method will call gs.get() to decide.
        box.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                boolean v=!gs.get(key);
                if (v) box.setIcon(on); else box.setIcon(off);
                gs.set(key, v);
            }
        });
        box.setMaximumSize(box.getPreferredSize());
        box.setToolTipText(tip);
        jlabel=new JLabel(label);
        jlabel.setToolTipText(tip);
        add(box);
        add(jlabel);
        setAlignmentX(RIGHT_ALIGNMENT);
    }

    /** This method is called by Swing whenever this component needs to be painted. */
    @Override public void paint(Graphics p) {
        Icon old=box.getIcon();
        if (gs.get(key)) { if (old!=on) box.setIcon(on); } else { if (old!=off) box.setIcon(off); }
        super.paint(p);
    }
}
