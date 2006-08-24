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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.util.EventListener;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.event.TreeWillExpandListener;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;

import kodviz.util.Dbg;
import kodviz.util.ResourceManager;

@SuppressWarnings("serial")
public class AlloySwingUtilities {

    /** This method returns true iff running on a Mac
    and look and feel is Aqua **/
    public static boolean onMac() {
        return System.getProperty("mrj.version") != null
            && UIManager.getSystemLookAndFeelClassName().equals(
                UIManager.getLookAndFeel().getClass().getName());
    }

    /** These methods wrap the invokeAndWait() and invokeLater() methods
    of SwingUtilities in order to catch all exceptions and handle them
    through the handleException() method **/

    public static void invokeAndWait(final Runnable r)
    throws InterruptedException,
           InvocationTargetException
    {
    SwingUtilities.invokeAndWait(new Runnable() {
        public void run() {
            try {
            r.run();
            } catch (Exception thrown_) {
            handleException(thrown_);
            }
        }
        });
    }

    public static void invokeLater(final Runnable r)
    {
    SwingUtilities.invokeLater(new Runnable() {
        public void run() {
            try {
            r.run();
            } catch (Exception thrown_) {
            handleException(thrown_);
            }
        }
        });
    }

    /** This method wraps the actionPerformed() method of an Action object
    in order to catch all exceptions and handle them through the
    handleException() method **/

    public static Action action(final Action a) {
    return new AbstractAction() {
        public void actionPerformed(ActionEvent e) {
            try {
            a.actionPerformed(e);
            } catch (Exception thrown) {
            handleException(thrown);
            }
        }

        public void addPropertyChangeListener(PropertyChangeListener l) {
            a.addPropertyChangeListener(l);
        }

        public Object getValue(String key) {
            return a.getValue(key);
        }

        public boolean isEnabled() {
            return a.isEnabled();
        }

        public void putValue(String key, Object value) {
            a.putValue(key, value);
        }

        public void removePropertyChangeListener(PropertyChangeListener l) {
            a.removePropertyChangeListener(l);
        }

        public void setEnabled(boolean b) {
            a.setEnabled(b);
        }
        };
    }

    /** This method wraps all methods of objects which subclass EventListener
    in order to catch all exceptions and handle them through the
    handelException() method.  If the object passed to this method belongs to
    a subclass of EventListener which is not yet implemented in this method,
    an IllegalArgumentException is passed to the handleException() method **/

    public static EventListener listener(EventListener l) {
    if (l instanceof ActionListener) {
        final ActionListener al = (ActionListener)l;
        return new ActionListener() {
            public void actionPerformed(ActionEvent e) {
            try {
                al.actionPerformed(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof AncestorListener) {
        final AncestorListener al = (AncestorListener)l;
        return new AncestorListener() {
            public void ancestorAdded(AncestorEvent e) {
            try {
                al.ancestorAdded(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void ancestorMoved(AncestorEvent e) {
            try {
                al.ancestorMoved(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void ancestorRemoved(AncestorEvent e) {
            try {
                al.ancestorRemoved(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof CaretListener) {
        final CaretListener cl = (CaretListener)l;
        return new CaretListener() {
            public void caretUpdate(CaretEvent e) {
            try {
                cl.caretUpdate(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof ComponentListener) {
        final ComponentListener cl = (ComponentListener)l;
        return new ComponentListener() {
            public void componentHidden(ComponentEvent e) {
            try {
                cl.componentHidden(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void componentMoved(ComponentEvent e) {
            try {
                cl.componentMoved(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void componentResized(ComponentEvent e) {
            try {
                cl.componentResized(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void componentShown(ComponentEvent e) {
            try {
                cl.componentShown(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof DocumentListener) {
        final DocumentListener dl = (DocumentListener)l;
        return new DocumentListener() {
            public void changedUpdate(DocumentEvent e) {
            try {
                dl.changedUpdate(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void insertUpdate(DocumentEvent e) {
            try {
                dl.insertUpdate(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void removeUpdate(DocumentEvent e) {
            try {
                dl.removeUpdate(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof FocusListener) {
        final FocusListener fl = (FocusListener)l;
        return new FocusListener() {
            public void focusGained(FocusEvent e) {
            try {
                fl.focusGained(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void focusLost(FocusEvent e) {
            try {
                fl.focusLost(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof HierarchyListener) {
        final HierarchyListener hl = (HierarchyListener)l;
        return new HierarchyListener() {
            public void hierarchyChanged(HierarchyEvent e) {
            try {
                hl.hierarchyChanged(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof InternalFrameListener) {
        final InternalFrameListener ifl = (InternalFrameListener)l;
        return new InternalFrameListener() {
            public void internalFrameActivated(InternalFrameEvent e) {
            try {
                ifl.internalFrameActivated(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void internalFrameClosed(InternalFrameEvent e) {
            try {
                ifl.internalFrameClosed(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void internalFrameClosing(InternalFrameEvent e) {
            try {
                ifl.internalFrameClosing(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void internalFrameDeactivated(InternalFrameEvent e) {
            try {
                ifl.internalFrameDeactivated(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void internalFrameDeiconified(InternalFrameEvent e) {
            try {
                ifl.internalFrameDeiconified(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void internalFrameIconified(InternalFrameEvent e) {
            try {
                ifl.internalFrameIconified(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void internalFrameOpened(InternalFrameEvent e) {
            try {
                ifl.internalFrameOpened(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof ItemListener) {
        final ItemListener il = (ItemListener)l;
        return new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
            try {
                il.itemStateChanged(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof KeyListener) {
        final KeyListener kl = (KeyListener)l;
        return new KeyListener() {
            public void keyPressed(KeyEvent e) {
            try {
                kl.keyPressed(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void keyReleased(KeyEvent e) {
            try {
                kl.keyReleased(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void keyTyped(KeyEvent e) {
            try {
                kl.keyTyped(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof ListSelectionListener) {
        final ListSelectionListener lsl = (ListSelectionListener)l;
        return new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
            try {
                lsl.valueChanged(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof MouseListener) {
        final MouseListener ml = (MouseListener)l;
        return new MouseListener() {
            public void mouseClicked(MouseEvent e) {
            try {
                ml.mouseClicked(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void mouseEntered(MouseEvent e) {
            try {
                ml.mouseEntered(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void mouseExited(MouseEvent e) {
            try {
                ml.mouseExited(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void mousePressed(MouseEvent e) {
            try {
                ml.mousePressed(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void mouseReleased(MouseEvent e) {
            try {
                ml.mouseReleased(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof PropertyChangeListener) {
        final PropertyChangeListener pcl = (PropertyChangeListener)l;
        return new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent e) {
            try {
                pcl.propertyChange(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof TableModelListener) {
        final TableModelListener tml = (TableModelListener)l;
        return new TableModelListener() {
            public void tableChanged(TableModelEvent e) {
            try {
                tml.tableChanged(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof TreeSelectionListener) {
        final TreeSelectionListener tsl = (TreeSelectionListener)l;
        return new TreeSelectionListener() {
            public void valueChanged(TreeSelectionEvent e) {
            try {
                tsl.valueChanged(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof TreeWillExpandListener) {
        final TreeWillExpandListener twel = (TreeWillExpandListener)l;
        return new TreeWillExpandListener() {
            public void treeWillCollapse(TreeExpansionEvent e) {
            try {
                twel.treeWillCollapse(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void treeWillExpand(TreeExpansionEvent e) {
            try {
                twel.treeWillExpand(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof UndoableEditListener) {
        final UndoableEditListener uel = (UndoableEditListener)l;
        return new UndoableEditListener() {
            public void undoableEditHappened(UndoableEditEvent e) {
            try {
                uel.undoableEditHappened(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else if (l instanceof WindowListener) {
        final WindowListener wl = (WindowListener)l;
        return new WindowListener() {
            public void windowActivated(WindowEvent e) {
            try {
                wl.windowActivated(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void windowClosed(WindowEvent e) {
            try {
                wl.windowClosed(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void windowClosing(WindowEvent e) {
            try {
                wl.windowClosing(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void windowDeactivated(WindowEvent e) {
            try {
                wl.windowDeactivated(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void windowDeiconified(WindowEvent e) {
            try {
                wl.windowDeiconified(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void windowIconified(WindowEvent e) {
            try {
                wl.windowIconified(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
            public void windowOpened(WindowEvent e) {
            try {
                wl.windowOpened(e);
            } catch (Exception thrown) {
                handleException(thrown);
            }
            }
        };
    } else {
        RuntimeException ex = new IllegalArgumentException("no wrapper implemented for "+
                                   l.getClass());
        handleException(ex);

        throw ex;  // this statement should never be reached,
                   // but this else branch must throw or return something
    }
    }

    private static void handleException(Exception thrown) {
	ByteArrayOutputStream os = new ByteArrayOutputStream();
	thrown.printStackTrace(new PrintStream(os));
        Dbg.fatal("GUI Error:\n"+os, thrown);
    }

    public static Border createBottomBorder() {
        return new BottomBorder();
    }

    public static Border createBottomBorder(Color color) {
        return new BottomBorder(color);
    }

    public static int getScreenWidth() {
        return (int)Toolkit.getDefaultToolkit().getScreenSize().getWidth();
    }

    public static int getScreenHeight() {
        return (int)Toolkit.getDefaultToolkit().getScreenSize().getHeight();
    }

    public static Image loadImage(String pathName) {
        Image image = ResourceManager.getImage(pathName);

        if (image == null) {
            Dbg.warn("Couldn't load image " + pathName);
            return new BufferedImage(5, 5, BufferedImage.TYPE_INT_RGB);
        }
        else {
            return image;
        }
    }

    public static int getShortcutKeyMask() {
	return Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
    }

    private static class BottomBorder extends CompoundBorder {

		private static final long serialVersionUID = 1L;

		BottomBorder() {
            super(new EmptyBorder(0, 0, 1, 0), new BottomLineBorder());
        }

        BottomBorder(Color color) {
            super(new EmptyBorder(0, 0, 1, 0), new BottomLineBorder(color));
        }
    }

    private static class BottomLineBorder implements Border {
        private Color color;

        BottomLineBorder() {
            color = Color.gray;
        }

        BottomLineBorder(Color theColor) {
            color = theColor;
        }

        public Insets getBorderInsets(Component c) {
            return new Insets(0, 0, 1, 0);
        }

        public boolean isBorderOpaque() {
            return true;
        }

        public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
            g.setColor(color);
            g.drawLine(x, y + height, x + width, y + height);
        }
    }
}
