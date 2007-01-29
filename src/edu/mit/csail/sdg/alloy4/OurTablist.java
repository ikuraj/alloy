package edu.mit.csail.sdg.alloy4;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.sun.org.apache.xerces.internal.impl.RevalidationHandler;

import static java.awt.Color.BLACK;
import static java.awt.Color.WHITE;

public final class OurTablist {

    private static final long serialVersionUID = 1L;

    private int me=0;
    private final JPanel frame;
    private final JScrollPane scroller;
    private final JPanel content;

    private static final class Tab {
        public JPanel panel;
        public JLabel label;
        public Component body;
        public Tab(JPanel panel, JLabel label, Component body) { this.panel=panel; this.label=label; this.body=body; }
    }

    private final List<Tab> list = new ArrayList<Tab>();

    private static final Color gray=new Color(.9f, .9f, .9f);
    private static final Color inactive=new Color(.8f, .8f, .8f);
    private static final Color border=BLACK; // Color.LIGHT_GRAY;

    public Container getContainer() { return frame; }

    public OurTablist() {
        content=new JPanel();
        content.setOpaque(true);
        content.setBackground(gray);
        content.setBorder(new EmptyBorder(0,0,0,0));
        content.setLayout(new BoxLayout(content, BoxLayout.X_AXIS));
        JPanel glue = new JPanel();
        glue.setLayout(new BoxLayout(glue, BoxLayout.X_AXIS));
        glue.add(Box.createHorizontalGlue());
        glue.setBorder(new OurBorder(null,null,border,null));
        glue.setAlignmentX(0.0f);
        glue.setAlignmentY(1.0f);
        content.add(glue);
        scroller=new JScrollPane(content, JScrollPane.VERTICAL_SCROLLBAR_NEVER, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scroller.setBorder(new EmptyBorder(0,0,0,0));
        scroller.addComponentListener(new ComponentListener() {
            public void componentResized(ComponentEvent e) {
            	setSelectedIndex(me);
            	/*for(Tab t:list) {
            		Dimension d=t.body.getSize();
            		d.width=frame.getWidth();
            		t.body.setSize(d);
            	}*/
            }
            public void componentMoved(ComponentEvent e) {setSelectedIndex(me);}
            public void componentShown(ComponentEvent e) {setSelectedIndex(me);}
            public void componentHidden(ComponentEvent e) {}
        });
        frame=new JPanel();
        frame.setBorder(new EmptyBorder(0,0,0,0));
        frame.setLayout(new BorderLayout());
        frame.add(scroller, BorderLayout.NORTH);
        frame.add(new JTextArea(), BorderLayout.CENTER);
//        addx(null, "ordering");
//        addx(null, "dijkstra");
//        addx(null, "go");
//        addx(null, "boolean");
//        addx(null, "void");
    }

    public void insertTab(String label, Icon unused, Component c, String tooltip, int position) {
        label=label.trim();
        if (label.length()>6) label="  "+label.substring(0,6)+"...  "; else label="  "+label+"  ";
        final JLabel x=new JLabel(label);
        x.setFont(OurUtil.getVizFont());
        x.setOpaque(true);
        x.setBorder(new OurBorder(border,border,WHITE,border));
        x.setBackground(WHITE);
        x.setForeground(BLACK);
        x.addMouseListener(new MouseListener() {
            public void mouseClicked(MouseEvent e) {
                for(int i=0;i<list.size();i++) if (list.get(i).label==x) {setSelectedIndex(i); break;}
                for(ChangeListener x:listeners) x.stateChanged(new ChangeEvent(OurTablist.this));
            }
            public void mousePressed(MouseEvent e) {}
            public void mouseReleased(MouseEvent e) {}
            public void mouseEntered(MouseEvent e) {}
            public void mouseExited(MouseEvent e) {}
        });
        JPanel h4=OurUtil.makeBox(4, 1);
        h4.setAlignmentX(0.0f);
        h4.setAlignmentY(1.0f);
        h4.setBorder(new OurBorder(null,null,border,null));
        JPanel h2=OurUtil.makeBox(3, 1);
        h2.setAlignmentX(0.0f);
        h2.setAlignmentY(1.0f);
        h2.setBorder(new OurBorder(null,null,border,null));
        JPanel xx=OurUtil.makeVL(null, 2, OurUtil.makeHB(h4, x, h2, gray), gray);
        xx.setAlignmentX(0.0f);
        xx.setAlignmentY(1.0f);
        content.add(xx, list.size());
        list.add(new Tab(xx,x,c));
        setSelectedIndex(list.size()-1);
    }

    public int getTabCount() { return list.size(); }

    private final List<ChangeListener> listeners=new ArrayList<ChangeListener>();

    public void removeChangeListener(ChangeListener listener) {
        for(int i=listeners.size()-1; i>=0; i--) {
            if (listeners.get(i)==listener) {
                listeners.remove(i);
                return;
            }
        }
    }

    public void addChangeListener(ChangeListener listener) {
        removeChangeListener(listener);
        listeners.add(listener);
    }

    public int getSelectedIndex() { return me; }

    public Component getComponentAt(int i) {
        if (i<0 || i>=list.size()) return new JPanel();
        return list.get(i).body;
    }

    public void removeTabAt(int i) {
        if (i<0 || i>=list.size()) return;
        list.remove(i);
        content.remove(i);
        if (me>=list.size()) me=list.size()-1;
        if (me<0) me=0;
        setSelectedIndex(me);
        frame.repaint();
        for(ChangeListener x:listeners) x.stateChanged(new ChangeEvent(OurTablist.this));
    }

    public void setTitleAt(int i, String x) {
        if (i<0 || i>=list.size()) return;
        x=x.trim();
        if (x.length()>6) x="  "+x.substring(0,6)+"...  "; else x="  "+x+"  ";
        list.get(i).label.setText(x);
    }

    public void setSelectedIndex(int i) {
        if (i<0 || i>=list.size()) return;
        me=i;
        for(int j=0; j<list.size(); j++) {
            JLabel x=list.get(j).label;
            if (j!=i) x.setBorder(new OurBorder(border,border,border,border)); else x.setBorder(new OurBorder(border,border,WHITE,border));
            if (j!=i) x.setBackground(inactive); else x.setBackground(WHITE);
            if (j!=i) continue;
            Point p=list.get(j).panel.getLocation();
            Dimension r=list.get(j).panel.getSize();
            content.scrollRectToVisible(new Rectangle(p.x, 0, r.width, 1));
            frame.removeAll();
            frame.add(scroller, BorderLayout.NORTH);
            frame.add(list.get(j).body, BorderLayout.CENTER);
            frame.revalidate();
        }
        frame.repaint();
        list.get(i).body.requestFocusInWindow();
    }
}
