package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * Graphical on/off checkbox.
 *
 * <p> To construct a OurBinaryCheckbox object, you need to give it a GetterSetter object and a key.
 * <br> Whenever the checkbox needs to be repainted, it calls gs.get(key) to find out whether it is ON or OFF.
 * <br> Whenever the checkbox is clicked by the user, it calls gs.set(key,value) to set a new value.
 *
 * <p> In other words, the checkbox does not contain any hidden states: all changes are immediately
 * committed into the data store, and it always re-queries the data store whenever it needs to repaint itself.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class OurBinaryCheckbox extends JPanel {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1;

    /** The icon to use when the checkbox is off. */
    private static final Icon off = OurUtil.loadIcon("images/cb0.gif");

    /** The icon to use when the checkbox is on. */
    private static final Icon on = OurUtil.loadIcon("images/cb1.gif");

    /** The underlying JCheckBox object. */
    private final JCheckBox box;

    /** The JLabel object for displaying a label next to the checkbox. */
    private final JLabel jlabel;

    /** The identifier associated with this checkbox. */
    private final Object key;

    /**
     * The GetterSetter associated with this checkbox.
     * <p> When the checkbox needs to be repainted, we call gs.get(key) to find out whether it is ON or OFF.
     * <p> When the checkbox is clicked by the user, we call gs.set(key,value) to set a new value.
     */
    private final BinaryGetterSetter gs;

    /**
     * This defines get/set methods.
     *
     * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
     */
    public interface BinaryGetterSetter {
        /** This method reads the boolean value associated with the key; this must be called only by the AWT event thread. */
        public boolean get(Object key);
        /** This method sets the boolean value associated with the key; this must be called only by the AWT event thread. */
        public void set(Object key, boolean value);
    }

    /**
     * Constructs a new binary checkbox.
     * @param gs - the GetterSetter associated with this checkbox
     * @param key - the key associated with this checkbox
     * @param label - the label to display next to the checkbox
     * @param tip - the tool tip to show when the mouse hovers over this check box
     */
    public OurBinaryCheckbox(final BinaryGetterSetter gs, final Object key, String label, String tip) {
        super();
        this.gs=gs;
        this.key=key;
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        box = new JCheckBox(on); // Doesn't matter if it's on or off; the paint() method will call gs.get() to decide.
        box.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                boolean v=!gs.get(key);
                box.setIcon(v?on:off);
                gs.set(key, v);
            }
        });
        box.setMaximumSize(box.getPreferredSize());
        box.setToolTipText(tip);
        jlabel=new JLabel(label);
        jlabel.setToolTipText(tip);
        jlabel.setFont(OurUtil.getVizFont());
        add(box);
        add(jlabel);
        setAlignmentX(RIGHT_ALIGNMENT);
    }

    /** This method is called by Swing to enable/disable a component. */
    @Override public void setEnabled(boolean enabled) {
        if (box!=null) {
            box.setEnabled(enabled);
        }
        if (jlabel!=null) {
            jlabel.setEnabled(enabled);
        }
    }

    /** This method is called by Swing to change its background color. */
    @Override public void setBackground(Color color) {
        super.setBackground(color);
        if (box!=null) {
            box.setBackground(color);
        }
        if (jlabel!=null) {
            jlabel.setBackground(color);
        }
    }

    /** This method is called by Swing whenever this component needs to be painted. */
    @Override public void paint(Graphics p) {
        Icon newIcon = gs.get(key) ? on : off;
        if (box.getIcon() != newIcon) {
            box.setIcon(newIcon);
        }
        super.paint(p);
    }
}
