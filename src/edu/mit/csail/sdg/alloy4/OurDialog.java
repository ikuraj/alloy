/* Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify,
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
 * OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Locale;
import java.awt.BorderLayout;
import java.awt.FileDialog;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.event.KeyListener;
import java.awt.event.KeyEvent;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.filechooser.FileFilter;
import static javax.swing.JOptionPane.YES_NO_OPTION;
import static javax.swing.JOptionPane.YES_NO_CANCEL_OPTION;
import static javax.swing.JOptionPane.WARNING_MESSAGE;
import static javax.swing.JOptionPane.QUESTION_MESSAGE;

/** Graphical dialog methods for asking the user some questions.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class OurDialog {

   /** The constructor is private, since this utility class never needs to be instantiated. */
   private OurDialog() { }

   /** Popup the given error message. */
   public static void alert(Frame parentFrame, Object message) {
      JOptionPane.showMessageDialog(parentFrame, message, "Error", JOptionPane.PLAIN_MESSAGE);
   }

   /** Popup the given error message, then terminate the program. */
   public static void fatal(Frame parentFrame, Object message) {
      try {
         JOptionPane.showMessageDialog(parentFrame, message, "Fatal Error", JOptionPane.ERROR_MESSAGE);
      } finally {
         System.exit(1);
      }
   }

   /** Ask if the user wishes to save the file, discard the file, or cancel the entire operation (default is cancel).
    * @return 'c' if cancel, 's' if save, 'd' if discard
    */
   public static char askSaveDiscardCancel(Frame parentFrame, String description) {
      int ans = JOptionPane.showOptionDialog(parentFrame,
         new String[] {
            description + " has not been saved. Do you want to",
            "cancel the operation, close the file without saving, or save it and close?"
         },
         "Warning",
         YES_NO_CANCEL_OPTION,
         WARNING_MESSAGE,
         null,
         new Object[] {"Save", "Don't Save", "Cancel"},
         "Cancel"
      );
      return (ans == JOptionPane.YES_OPTION) ? 's' : (ans == JOptionPane.NO_OPTION ? 'd' : 'c');
   }

   /**
    * Ask if the user really wishes to overwrite the file (default is no).
    * @return true if the user wishes to overwrite the file, false if the user does not wish to overwrite the file.
    */
   public static boolean askOverwrite(Frame parentFrame, String filename) {
      int ans=JOptionPane.showOptionDialog(parentFrame,
            new String[] {"The file \"" + filename + "\"", "already exists. Do you wish to overwrite it?"},
            "Warning: file already exists",
            YES_NO_OPTION,
            WARNING_MESSAGE,
            null,
            new Object[] {"Overwrite", "Cancel"},
            "Cancel"
      );
      return ans == JOptionPane.YES_OPTION;
   }

   /** This caches the result of the call to get all fonts. */
   private static String[] allFonts = null;

   /** Returns true if a font with that name exists on the system. */
   public synchronized static boolean hasFont(String fontname) {
      if (allFonts == null) allFonts = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
      for(int i = 0; i < allFonts.length; i++) if (fontname.compareToIgnoreCase(allFonts[i]) == 0) return true;
      return false;
   }

   /** Asks the user to choose a font; returns "" if the user cancels the request. */
   public synchronized static String askFont(Frame parentFrame) {
      if (allFonts == null) allFonts = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
      JComboBox jcombo = new OurCombobox(allFonts);
      int ans = JOptionPane.showOptionDialog(
            parentFrame,
            new Object[] {"Please choose the new font:", jcombo},
            "Font",
            YES_NO_OPTION,
            WARNING_MESSAGE,
            null,
            new Object[] {"Ok", "Cancel"},
            "Cancel"
      );
      Object value = jcombo.getSelectedItem();
      if (ans != JOptionPane.YES_OPTION || !(value instanceof String)) return ""; else return (String)value;
   }

   /** True if we should use AWT (instead of Swing) to display the OPEN and SAVE dialog. */
   private static boolean useAWT = Util.onMac();

   /** Use the platform's preferred file chooser to ask the user to select a file.
    * <br> Note: if it is a save operation, and the user didn't include an extension, then we'll add the extension.
    * @param parentFrame - the parent frame
    * @param isOpen - true means this is an Open operation; false means this is a Save operation
    * @param dir - the initial directory (or null if we want to use the default)
    * @param ext - the file extension (including "."; using lowercase letters; for example, ".als") or ""
    * @param description - the description for the given extension
    * @return null if the user didn't choose anything, otherwise it returns the selected file
    */
   public static File askFile (Frame parentFrame, boolean isOpen, String dir, final String ext, final String description) {
      if (dir == null) dir = Util.getCurrentDirectory();
      if (!(new File(dir).isDirectory())) dir = System.getProperty("user.home");
      dir = Util.canon(dir);
      String ans;
      if (useAWT) {
         FileDialog open = new FileDialog(parentFrame, isOpen ? "Open..." : "Save...");
         open.setMode(isOpen ? FileDialog.LOAD : FileDialog.SAVE);
         open.setDirectory(dir);
         if (ext.length()>0) open.setFilenameFilter(new FilenameFilter() {
            public boolean accept(File dir, String name) { return name.toLowerCase(Locale.US).endsWith(ext); }
         });
         open.setVisible(true); // This method blocks until the user either chooses something or cancels the dialog.
         if (open.getFile() == null) return null;
         ans = open.getDirectory() + File.separatorChar + open.getFile();
      } else {
         try {
            JFileChooser open = new JFileChooser(dir);
            open.setDialogTitle(isOpen ? "Open..." : "Save...");
            open.setApproveButtonText(isOpen ? "Open" : "Save");
            if (ext.length()>0) open.setFileFilter(new FileFilter() {
               public boolean accept(File file) { return !file.isFile() || file.getPath().toLowerCase(Locale.US).endsWith(ext); }
               public String getDescription() { return description; }
            });
            if (open.showOpenDialog(parentFrame) != JFileChooser.APPROVE_OPTION || open.getSelectedFile() == null) return null;
            ans = open.getSelectedFile().getPath();
         } catch(Exception ex) {
            // Some combination of Windows version and JDK version will trigger this failure.
            // In such a case, we'll fall back to using the "AWT" file open dialog
            useAWT = true;
            return askFile(parentFrame, isOpen, dir, ext, description);
         }
      }
      if (!isOpen) {
         int lastSlash = ans.lastIndexOf(File.separatorChar);
         int lastDot = (lastSlash>=0) ? ans.indexOf('.', lastSlash) : ans.indexOf('.');
         if (lastDot<0) ans = ans + ext;
      }
      return new File(Util.canon(ans));
   }

   /** Display "msg" in a modal dialog window, and ask the user to choose "yes" versus "no" (default is "no"). */
   public static boolean yesno(Frame parentFrame, String msg, String yes, String no) {
      return JOptionPane.YES_OPTION == JOptionPane.showOptionDialog(
            parentFrame, msg, "Question", YES_NO_OPTION, WARNING_MESSAGE, null, new Object[]{yes, no}, no
      );
   }

   /** Display "msg" in a modal dialog window, and ask the user to choose "Yes" versus "No" (default is "no"). */
   public static boolean yesno(Frame parentFrame, String msg) {
      return yesno(parentFrame, msg, "Yes", "No");
   }

   /** Display a modal dialog window containing the "objects"; returns true iff the user clicks Ok. */
   public static boolean getInput(Frame parentFrame, String title, Object... objects) {
      // If there is a JTextField or a JTextArea here, then let the first JTextField or JTextArea be the initially focused widget
      Object main = "Ok";
      for(Object obj: objects) if (obj instanceof JTextField || obj instanceof JTextArea) { main = obj; break; }
      // Construct the dialog panel
      final JOptionPane pane = new JOptionPane(objects, QUESTION_MESSAGE, YES_NO_OPTION, null, new Object[]{"Ok", "Cancel"}, main);
      final JDialog dialog = pane.createDialog(parentFrame, title);
      // For each JTextField and JCheckBox, add a KeyListener that detects VK_ENTER and treat it as if the user clicked OK
      for(Object obj: objects) if (obj instanceof JTextField || obj instanceof JCheckBox) {
         ((JComponent)obj).addKeyListener(new KeyListener() {
            public void keyPressed(KeyEvent e)  { if (e.getKeyCode()==KeyEvent.VK_ENTER) { pane.setValue("Ok"); dialog.dispose(); } }
            public void keyReleased(KeyEvent e) { }
            public void keyTyped(KeyEvent e)    { }
         });
      }
      dialog.setVisible(true); // This method blocks until the user either chooses something or cancels the dialog.
      return pane.getValue() == "Ok";
   }

   /** Display a simple non-modal window showing some text. */
   public static JFrame showtext(String title, String text) {
      JFrame window = new JFrame(title);
      JButton done = new JButton("Close");
      done.addActionListener(Runner.createDispose(window));
      JScrollPane scrollPane = OurUtil.scrollpane(OurUtil.textarea(text, 20, 60, false, false));
      window.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
      window.getContentPane().setLayout(new BorderLayout());
      window.getContentPane().add(scrollPane, BorderLayout.CENTER);
      window.getContentPane().add(done, BorderLayout.SOUTH);
      window.pack();
      window.setSize(500, 500);
      window.setLocationRelativeTo(null);
      window.setVisible(true);
      return window;
   }
}
