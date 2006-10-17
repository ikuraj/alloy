package edu.mit.csail.sdg.alloy4util;

import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

public final class OurBinaryCheckbox extends JPanel {

    private static final long serialVersionUID = 1;
    private static final ImageIcon off = OurUtil.loadIcon("images/cb0.gif");
    private static final ImageIcon on  = OurUtil.loadIcon("images/cb1.gif");
    private final JCheckBox box;
    private final JLabel jlabel;
    private final GetterSetter gs;
    private final Object key;

    public interface GetterSetter {
        public boolean get(Object key);
        public void set(Object key, boolean value);
    }

    public OurBinaryCheckbox(final GetterSetter gs, final Object key, String label, String tip) {
        super();
        this.gs=gs;
        this.key=key;
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        box = new JCheckBox(off); // This value doesn't matter
        box.addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { setBinaryState(!gs.get(key)); }
        });
        box.setMaximumSize(box.getPreferredSize());
        box.setToolTipText(tip);
        jlabel=new JLabel(label);
        jlabel.setToolTipText(tip);
        add(box);
        add(jlabel);
        setAlignmentX(RIGHT_ALIGNMENT);
    }

    @Override public void paint(Graphics p) {
        Icon old=box.getIcon();
        if (gs.get(key)) { if (old!=on) box.setIcon(on); } else { if (old!=off) box.setIcon(off); }
        super.paint(p);
    }

    private void setBinaryState(boolean v) {
        boolean old=gs.get(key);
        if (v) box.setIcon(on); else box.setIcon(off);
        if (old==v) return;
        gs.set(key, v);
    }
}
