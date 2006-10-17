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

public final class OurTristateCheckbox extends JPanel {

    private static final long serialVersionUID = 1;

    private static final ImageIcon off  = OurUtil.loadIcon("images/tcb01.gif");
    private static final ImageIcon on   = OurUtil.loadIcon("images/tcb02.gif");
    private static final ImageIcon ioff = OurUtil.loadIcon("images/tcb03.gif");
    private static final ImageIcon ion  = OurUtil.loadIcon("images/tcb04.gif");
    private final JCheckBox box;
    private final JLabel jlabel;
    private final Object key;
    private final GetterSetter gs;

    public interface GetterSetter {
        public Boolean get(Object key);
        public boolean getInherited(Object key);
        public void set(Object key,Boolean value);
    }

    public OurTristateCheckbox(final GetterSetter gs, final Object key, String label, String tip) {
        super();
        this.key=key;
        this.gs=gs;
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        box = new JCheckBox(off); // This value doesn't matter.
        box.addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) {
                Boolean value=gs.get(key);
                if (value==null) setTriState(Boolean.TRUE);
                else if (value.booleanValue()) setTriState(Boolean.FALSE);
                else setTriState(null);
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

    private void setTriState(Boolean v) {
        Boolean old=gs.get(key); // getTriState();
        if (v==null) {
            if (gs.getInherited(key)) box.setIcon(ion); else box.setIcon(ioff);
            if (old==null) return;
        } else {
            if (v.booleanValue()) box.setIcon(on); else box.setIcon(off);
            if (old!=null && old.booleanValue()==v.booleanValue()) return;
        }
        gs.set(key,v);
    }
}
