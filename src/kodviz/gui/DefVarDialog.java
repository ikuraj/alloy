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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.UIManager;
import javax.swing.border.Border;

import kodviz.alloyviz.CustVar;


public class DefVarDialog extends JDialog {

	private static final long serialVersionUID = 1L;

	private static final String VAR_LBL = "Variable name:";
    private static final String EXPR_LBL = "Expression:";
    private static final String OK_BTN_LBL = "OK";
    private static final String CANCEL_BTN_LBL = "Cancel";
    private static final String TITLE = "Define Custom Variable";

    private Border WIDGET_BORDER = BorderFactory.createEmptyBorder(5, 5, 5, 5);

    private JTextField varText;
    private JTextArea exprText;

    private JTextArea warningLbl;

    private ModulePanel _parent;      

    private JPanel mainPanel;
    
    private boolean _editingMode;
    private CustVar _editedCV;
    private boolean _isEditedRelation;
    private int _editedEltIndex;

    /**
     * Defaults to this(parent_, false, null, false -1);
     * 
     */
    public DefVarDialog(ModulePanel parent_) {
        this(parent_, false, null, false, -1);
    }
    
    /**
     * 
     * @param parent_
     * @param editingMode if true, name textbox is non-editable and boxes are loaded with data in editedCV
     * @param editedCV if editingMode is true, put a non-null object here
     * @param isEditedRelation true if editd elt is an AlloyRelation
     * @param editedEltIndex index of edited elt on its switchable panel
     */
    public DefVarDialog(ModulePanel parent_, boolean editingMode, CustVar editedCV, boolean isEditedRelation, int editedEltIndex) {
        super();
        setTitle(TITLE);
        setModal(true);

        _editingMode = editingMode;
        _editedCV = editedCV;
        _isEditedRelation = isEditedRelation;
        _editedEltIndex = editedEltIndex;        
        
        _parent = parent_;
        Container cp = getContentPane();
        cp.setLayout(new BorderLayout());
        warningLbl = new JTextArea("",2,28);
        warningLbl.setEditable(false);
        warningLbl.setLineWrap(true);
        warningLbl.setWrapStyleWord(true);
        warningLbl.setFont(UIManager.getFont("Label.font"));
        warningLbl.setBackground(UIManager.getColor("Label.background"));
        warningLbl.setAlignmentX(LEFT_ALIGNMENT);

        JScrollPane warningScrollPane = new JScrollPane(warningLbl);
        warningScrollPane.setBorder(null);
        warningScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);

        //cp.add(warningLbl, BorderLayout.NORTH);

        mainPanel = new JPanel();

        //mainPanel.add(warningLbl);
        mainPanel.add(warningScrollPane);

        //mainPanel.setLayout(new GridLayout(2, 2));
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

        JPanel varPanel = new JPanel();
        varPanel.setLayout(new BoxLayout(varPanel, BoxLayout.X_AXIS));
        varPanel.add(new JLabel(VAR_LBL));
        varText = new JTextField(15);
        
        if (_editingMode) {
            varText.setText(editedCV.getName());
            varText.setEditable(false);            
        }
        
        //System.out.println(varText.getPreferredSize());
        varText.setMaximumSize(varText.getPreferredSize());
        varPanel.add(Box.createHorizontalGlue());
        varPanel.add(varText);
        varPanel.setAlignmentX(LEFT_ALIGNMENT);

        JPanel exprPanel = new JPanel();
        exprPanel.setLayout(new BoxLayout(exprPanel, BoxLayout.X_AXIS));
        exprPanel.add(new JLabel(EXPR_LBL));
        exprText = new JTextArea(2, 13);
        exprText.setLineWrap(true);
        exprText.setWrapStyleWord(true);
        exprText.setMaximumSize(exprText.getPreferredSize());
        
        if (_editingMode) {
            exprText.setText(editedCV.getExpr());
        }
        
        JScrollPane areaScrollPane = new JScrollPane(exprText);
        //areaScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        //System.out.println(exprText.getPreferredSize());
        exprPanel.add(Box.createHorizontalGlue());
        exprPanel.add(areaScrollPane);
        exprPanel.setAlignmentX(LEFT_ALIGNMENT);

        mainPanel.add(varPanel);
        mainPanel.add(Box.createRigidArea(new Dimension(0, 10)));
        mainPanel.add(exprPanel);

        JButton okBtn = new JButton(OK_BTN_LBL);
        okBtn.setMnemonic('O');

        this.getRootPane().setDefaultButton(okBtn);

        okBtn.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if (varText.getText().equals("") || exprText.getText().equals("")) {
                    warningLbl.setForeground(Color.RED);
                    warningLbl.setText("Fields must be non-empty");
                }
                else {                    
                    
                    String msg = (_editingMode ? 
                            _parent.editCustVar(_editedCV, exprText.getText(), _isEditedRelation, _editedEltIndex) : 
                                _parent.addCustVar(varText.getText(), exprText.getText()));

                    if (msg!=null) {
                        warningLbl.setForeground(Color.RED);
                        warningLbl.setText(msg);
                        //DefVarDialog.this.setSize(mainPanel.getPreferredSize());
                        //DefVarDialog.this.setSize(mainPanel.getPreferredSize());
                    } else {
                        setVisible(false);
                        warningLbl.setSize(warningLbl.getPreferredSize());
                        //DefVarDialog.this.setSize(mainPanel.getPreferredSize());
                    }
                }
            }
        });

        JButton cancelBtn = new JButton(CANCEL_BTN_LBL);
        cancelBtn.setMnemonic('C');
        cancelBtn.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                setVisible(false);
            }
        });

        JPanel btnPanel = new JPanel();
        btnPanel.add(okBtn);
        btnPanel.add(cancelBtn);
        btnPanel.setAlignmentX(LEFT_ALIGNMENT);

        mainPanel.add(Box.createRigidArea(new Dimension(0, 10)));
        mainPanel.add(btnPanel);

        mainPanel.setBorder(WIDGET_BORDER);

        mainPanel.setMinimumSize(new Dimension(300,200));

        cp.add(mainPanel);
        //cp.add(btnPanel, BorderLayout.SOUTH);
        setSize(300, 200);
        setResizable(true);
    }
}
