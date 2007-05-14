/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
 */

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
 * Graphical three-state checkbox.
 *
 * <p> To construct a OurTristateCheckbox object, you need to give it a GetterSetter object and a key.
 * <br> Whenever the checkbox needs to be repainted, it calls gs.get(key) to find out if it is ON or OFF or INHERIT,
 * <br> and it's INHERIT, it calls gs.getInherited(key) to find out if the inherited value is ON or OFF.
 * <br> Whenever the checkbox is clicked by the user, it calls gs.set(key,value) to set a new value.
 *
 * <p> In other words, the checkbox does not contain any hidden states: all changes are immediately
 * committed into the data store, and it always re-queries the data store whenever
 * it needs to know if it's on or off or inherited.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class OurTristateCheckbox extends JPanel {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1;

    /** The icon to use when the checkbox is off entirely. */
    private static final Icon off = OurUtil.loadIcon("images/tcb01.gif");

    /** The icon to use when the checkbox is on entirely. */
    private static final Icon on = OurUtil.loadIcon("images/tcb02.gif");

    /** The icon to use when the checkbox is off due to inheritance. */
    private static final Icon ioff = OurUtil.loadIcon("images/tcb03.gif");

    /** The icon to use when the checkbox is on due to inheritance. */
    private static final Icon ion = OurUtil.loadIcon("images/tcb04.gif");

    /** The underlying JCheckBox object. */
    private final JCheckBox box;

    /** The JLabel object for displaying a label next to the checkbox. */
    private final JLabel jlabel;

    /** The identifier associated with this checkbox. */
    private final Object key;

    /**
     * The GetterSetter associated with this checkbox.
     * <p> When the checkbox needs to be repainted, we call gs.get(key) to find out if it is ON or OFF or INHERIT.
     * <p> If it is INHERIT, we call gs.getInherited(key) to find out if the inherited value is ON or OFF.
     * <p> When the checkbox is clicked by the user, we call gs.set(key,value) to set a new value.
     */
    private final GetterSetter gs;

    /**
     * This defines get/getInherited/set methods.
     *
     * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
     */
    public interface GetterSetter {
        /**
         * This method reads the value associated with the key; this must be called only by the AWT event thread.
         * <br> If the answer is null, that means this key inherits its true or false value from its parent.
         */
        public Boolean get(Object key);
        /**
         * This method reads the value associated with the key; this must be called only by the AWT event thread.
         * <br> If this key inherits its value, this method will query its parent until we get either true or false.
         */
        public boolean getInherited(Object key);
        /**
         * This method sets the value associated with the key; this must be called only by the AWT event thread.
         * <br> If you want to tell this key to go into the "inherit" state, then use null as the value.
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
        box = new JCheckBox(on); // Doesn't matter if it's on or off; the paint() method will call gs.get() to decide.
        box.addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) {
                Boolean old=gs.get(key);
                if (old==null) {
                    gs.set(key,Boolean.TRUE);
                    box.setIcon(on);
                } else if (old.booleanValue()) {
                    gs.set(key,Boolean.FALSE);
                    box.setIcon(off);
                } else {
                    gs.set(key,null);
                    box.setIcon(gs.getInherited(key)?ion:ioff);
                }
            }
        });
        box.setMaximumSize(box.getPreferredSize());
        box.setToolTipText(tip);
        jlabel=new JLabel(label);
        jlabel.setToolTipText(tip);
        jlabel.setFont(OurUtil.getVizFont());
        add(jlabel);
        add(box);
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
        Icon newIcon;
        Boolean value=gs.get(key);
        if (value==null) {
            newIcon = gs.getInherited(key) ? ion : ioff;
        } else {
            newIcon = value.booleanValue() ? on : off;
        }
        if (box.getIcon()!=newIcon) box.setIcon(newIcon);
        super.paint(p);
    }
}
