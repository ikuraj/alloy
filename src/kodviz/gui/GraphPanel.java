/*
 * Alloy Analyzer
 * Copyright (c) 2002 Massachusetts Institute of Technology
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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.ListCellRenderer;

import kodviz.alloyviz.AlloyAtom;
import kodviz.alloyviz.AlloyType;
import kodviz.alloyviz.ProjectionFrame;
import kodviz.alloyviz.VizInstance;
import kodviz.dotviz.Visualizer;
import kodviz.graph.Cartoon;
import kodviz.graph.Indexer;


/**
 * The purpose of this class is to basically tack on the buttons for navigating
 * through projections to the basic graph JPanel returned by grappa. 
 */
class GraphPanel extends JPanel {

	private static final long serialVersionUID = 1L;

    private String LEFT_BUTTON_TEXT = "<<";
    private String RIGHT_BUTTON_TEXT = ">>";

    private ProjectionFrame _projFrame;
    private List _projTypes;
    private Map _typesToPanels;
    private Map _indexersToPanels;
    private JScrollPane _diagramScrollPanel;
    private JPanel _diagramPanel;
    private JPanel _navigationPanel;
    private Cartoon _cartoon;
    private Set _cachedIndexers;

    private JSplitPane split;
    /**
     * Creates a panel containing the diagram/graph as well as necessary projection
     * VCR buttons prescribed by the projFrame_.  Pass in the *OLD* instance_
     * (i.e. the unprojected one) so that the panel can determine all the set of
     * atoms corresponding to each projected type to display in the combo boxes.     
     */
    @SuppressWarnings("unchecked")
    GraphPanel(VizInstance instance_, ProjectionFrame projFrame_, Cartoon cartoon_) {
        this.setLayout(new GridLayout());
        this.setMaximumSize(new Dimension(Short.MAX_VALUE, Short.MAX_VALUE));
        //System.out.println("Generating graph\n");
        //setLayout(new BorderLayout());

         _projFrame = projFrame_;
        _projTypes = new ArrayList(_projFrame.getProjectedTypes());
        Collections.sort(_projTypes);
        _typesToPanels = new HashMap();
        _cartoon = cartoon_;

        _indexersToPanels = new HashMap();

        _diagramPanel = new JPanel();
        _diagramScrollPanel = new JScrollPane(_diagramPanel);
        _diagramPanel.setBorder(null); //WIDGET_BORDER);        
        //_diagramScrollPanel.setMinimumSize(new Dimension(400,400));       

        this.setBorder(null);

        _cachedIndexers = new HashSet();

        _navigationPanel = generateWidgets(instance_);

        split =
            new AlloySplitPane(JSplitPane.VERTICAL_SPLIT, _diagramScrollPanel, new JScrollPane(_navigationPanel));
        //this.setMaximumSize(new Dimension(Short.MAX_VALUE, Short.MAX_VALUE));
        split.setResizeWeight(1.0);
        split.setDividerSize(0);

        //add(_diagramPanel, BorderLayout.CENTER);
        //add(_navigationPanel, BorderLayout.SOUTH);
        add(split);

        updateProjected();
    }

    public void paint(Graphics g) {
        split.setDividerLocation(
            split.getSize().height
                - split.getInsets().bottom
                - split.getDividerSize()
                - split.getRightComponent().getPreferredSize().height
                );
    }

    @SuppressWarnings("unchecked")
    private JPanel generateWidgets(final VizInstance instance) {
        JPanel widgetPanel = new JPanel();
        for (Iterator types = _projTypes.iterator(); types.hasNext();) {
            AlloyType curType = (AlloyType)types.next();
            TypePanel temp = new TypePanel(new Vector(instance.getAtomsOfType(curType)));
            _typesToPanels.put(curType, temp);
            widgetPanel.add(temp);
        }
        return widgetPanel;
    }

    @SuppressWarnings("unchecked")
    private void updateProjected() {
        List indexers = new ArrayList();
        for (Iterator types = _projTypes.iterator(); types.hasNext();) {
            AlloyType curType = (AlloyType)types.next();
            TypePanel curPanel = (TypePanel)_typesToPanels.get(curType);
            indexers.add(new Indexer(curType.getName(), curPanel.getCurrentIndex()));
        }

        if (_cachedIndexers.contains(indexers)) {
            showGraph(indexers);
        } else {
            _cachedIndexers.add(indexers);
            JPanel cached = (new Visualizer()).generateGraphPanel(_cartoon, indexers);
            addGraph(cached, indexers);
            showGraph(indexers);
        }
        //System.out.println("In updateProjected()\n");
    }

    private void showGraph(Object key) {
        _diagramPanel.removeAll();
        _diagramPanel.add((JPanel)_indexersToPanels.get(key));
        _diagramPanel.validate();
        _diagramPanel.repaint();
    }

    @SuppressWarnings("unchecked")
    private void addGraph(JPanel graph, Object key) {
        _indexersToPanels.put(key, graph);
        graph.setBorder(null);
    }

    private class TypePanel extends JPanel {

		private static final long serialVersionUID = 1L;

		JButton leftBtn;
        JButton rightBtn;
        JComboBox atomCombo;

        @SuppressWarnings("unchecked")
        TypePanel(final Vector atoms) {
            super();

            this.setBorder(null); //WIDGET_BORDER);

            ListCellRenderer renderer = new AtomListCellRenderer();

            setLayout(new BoxLayout(this, BoxLayout.X_AXIS));

            Collections.sort(atoms);
            atomCombo = new JComboBox(atoms);
            atomCombo.setRenderer(renderer);
            leftBtn = new JButton(LEFT_BUTTON_TEXT);
            rightBtn = new JButton(RIGHT_BUTTON_TEXT);

            leftBtn.setEnabled(false);
            // i don't see why they would ever be generated when there's <= 1
            // element, but according to a bug report this was the case.
            if (atoms.size() <= 1) {
                rightBtn.setEnabled(false);
            }

            add(leftBtn);
            add(atomCombo);
            add(rightBtn);

            leftBtn.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    int curIndex = atomCombo.getSelectedIndex();
                    if (curIndex > 0) {
                        atomCombo.setSelectedIndex(curIndex - 1);
                    }
                    //updateProjected();
                }
            });

            rightBtn.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    int curIndex = atomCombo.getSelectedIndex();
                    if (curIndex < atomCombo.getItemCount() - 1) {
                        atomCombo.setSelectedIndex(curIndex + 1);
                    }
                    //updateProjected();
                }
            });
            atomCombo.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    //System.out.println("atomCombo event listener\n");
                    updateProjected();
                    leftBtn.setEnabled(getCurrentIndex() > 0);
                    rightBtn.setEnabled(getCurrentIndex() < atoms.size() - 1);
                    GraphPanel.this.getParent().validate();
                }
            });
        }

        int getCurrentIndex() {
            return atomCombo.getSelectedIndex();
        }
    }

}

class AtomListCellRenderer extends JLabel implements ListCellRenderer {

	private static final long serialVersionUID = 1L;

	AtomListCellRenderer() {
        super();
        setOpaque(true);
    }

    public Component getListCellRendererComponent(
        JList list,
        Object value,
        int index,
        boolean isSelected,
        boolean cellHasFocus) {

        if (value == null) {
            this.setText("No atoms");
        } else {
            this.setText(((AlloyAtom)value).getName());
        }
        if (isSelected) {
            setBackground(list.getSelectionBackground());
            setForeground(list.getSelectionForeground());
        } else {
            setBackground(list.getBackground());
            setForeground(list.getForeground());
        }
        return this;
    }
}
