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
 * Graphical three-state checkbox.
 *
 * <p/> To construct a OurTristateCheckbox object, you need to give it a GetterSetter object and a key.
 * <br/> Whenever the checkbox needs to be repainted, it calls gs.get(key) to find out if it is ON or OFF or INHERIT,
 * <br/> and it's INHERIT, it calls gs.getInherited(key) to find out if the inherited value is ON or OFF.
 * <br/> Whenever the checkbox is clicked by the user, it calls gs.set(key,value) to set a new value.
 *
 * <p/> In other words, the checkbox does not contain any hidden states: all changes are immediately
 * committed into the data store, and it always re-queries the data store whenever
 * it needs to know if it's on or off or inherited.
 *
 * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
 *
 * @author Felix Chang
 */

public final class OurTristateCheckbox extends JPanel {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1;

    /** The icon to use when the checkbox is off entirely. */
    private static final ImageIcon off  = OurUtil.loadIcon("images/tcb01.gif");

    /** The icon to use when the checkbox is on entirely. */
    private static final ImageIcon on   = OurUtil.loadIcon("images/tcb02.gif");

    /** The icon to use when the checkbox is off due to inheritance. */
    private static final ImageIcon ioff = OurUtil.loadIcon("images/tcb03.gif");

    /** The icon to use when the checkbox is on due to inheritance. */
    private static final ImageIcon ion  = OurUtil.loadIcon("images/tcb04.gif");

    /** The underlying JCheckBox object. */
    private final JCheckBox box;

    /** The JLabel object for displaying a label next to the checkbox. */
    private final JLabel jlabel;

    /** The identifier associated with this checkbox. */
    private final Object key;

    /**
     * The GetterSetter associated with this checkbox.
     * <p/> When the checkbox needs to be repainted, we call gs.get(key) to find out if it is ON or OFF or INHERIT.
     * <p/> If it is INHERIT, we call gs.getInherited(key) to find out if the inherited value is ON or OFF.
     * <p/> When the checkbox is clicked by the user, we call gs.set(key,value) to set a new value.
     */
    private final GetterSetter gs;

    /**
     * This interface defines the get(key), getInherited(key), and set(key,value) methods.
     *
     * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
     */
    public interface GetterSetter {
        /**
         * This method reads the value associated with the key; this must be called only by the AWT thread.
         * <br/> If the answer is null, that means this key inherits its true or false value from its parent.
         */
        public Boolean get(Object key);
        /**
         * This method reads the value associated with the key; this must be called only by the AWT thread.
         * <br/> If this key inherits its value, this method will query its parent until we get either true or false.
         */
        public boolean getInherited(Object key);
        /**
         * This method sets the value associated with the key; this must be called only by the AWT thread.
         * <br/> If you want to tell this key to go into the "inherit" state, then use null as the value.
         */
        public void set(Object key,Boolean value);
    }

    /**
     * Constructs a OurTristateCheckbox object.
     * @param gs - the GetterSetter associated with this checkbox
     * @param key - the key associated with this checkbox
     * @param label - the label to display next to the checkbox
     * @param tip - the tool tip to show when the mouse hovers over this checkbox
     */
    public OurTristateCheckbox(final GetterSetter gs, final Object key, String label, String tip) {
        super();
        this.key=key;
        this.gs=gs;
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        box = new JCheckBox(off); // Doesn't matter if it's on or off; the paint() method will call gs.get() to decide.
        box.addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) {
                Boolean old=gs.get(key);
                if (old==null) { box.setIcon(on); gs.set(key,Boolean.TRUE); }
                else if (old.booleanValue()) { box.setIcon(off); gs.set(key,Boolean.FALSE); }
                else if (gs.getInherited(key)) { box.setIcon(ion); gs.set(key,null); }
                else { box.setIcon(ioff); gs.set(key,null); }
            }
        });
        box.setMaximumSize(box.getPreferredSize());
        box.setToolTipText(tip);
        jlabel=new JLabel(label);
        jlabel.setToolTipText(tip);
        add(jlabel);
        add(box);
        setAlignmentX(RIGHT_ALIGNMENT);
    }

    /** This method is called by Swing whenever this component needs to be painted. */
    @Override public void paint(Graphics p) {
        Icon old=box.getIcon();
        Boolean value=gs.get(key);
        if (value==null) {
            if (gs.getInherited(key)) { if (old!=ion) box.setIcon(ion); } else { if (old!=ioff) box.setIcon(ioff); }
        } else {
            if (value.booleanValue()) { if (old!=on) box.setIcon(on); } else { if (old!=off) box.setIcon(off); }
        }
        super.paint(p);
    }
}
