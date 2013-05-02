package edu.mit.csail.sdg.alloy4whole;

import java.awt.Component;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractListModel;
import javax.swing.BoundedRangeModel;
import javax.swing.ComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.basic.BasicComboBoxRenderer;

import edu.mit.csail.sdg.alloy4.OurBorder;
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.OurUtil.GridBagConstraintsBuilder;
import edu.mit.csail.sdg.alloy4.Subprocess;
import edu.mit.csail.sdg.alloy4.Util.BooleanPref;
import edu.mit.csail.sdg.alloy4.Util.ChoicePref;
import edu.mit.csail.sdg.alloy4.Util.DelayedChoicePref;
import edu.mit.csail.sdg.alloy4.Util.IntChoicePref;
import edu.mit.csail.sdg.alloy4.Util.IntPref;
import edu.mit.csail.sdg.alloy4.Util.Pref;
import edu.mit.csail.sdg.alloy4.Util.StringChoicePref;
import edu.mit.csail.sdg.alloy4.Util.StringPref;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Options.SatSolver;

@SuppressWarnings("serial")
public class PreferencesDialog extends JFrame {

   private static final long serialVersionUID = 5577892964740788892L;
   private JTabbedPane tab;
//   private JPanel editorPane;
//   private JPanel solverPane;
//   private JPanel miscPane;
   
   @SuppressWarnings("unchecked")
   private static class CBModel<T> extends AbstractListModel implements ComboBoxModel {
      private final ChoicePref<T> pref;
      public CBModel(final ChoicePref<T> pref) { 
         this.pref = pref;
         this.pref.addChangeListener(new ChangeListener() {            
            public void stateChanged(ChangeEvent e) {
               fireContentsChanged(pref, -1, -1);
            }
         });
      }
      
      public int getSize()                       { return pref.validChoices().size(); }
      public Object getElementAt(int index)      { return pref.validChoices().get(index); }
      public void setSelectedItem(Object anItem) { pref.set((T)anItem); }
      public Object getSelectedItem()            { return pref.get(); }
   }

   private static class BRModel<T> implements BoundedRangeModel {
      private final ChoicePref<T> pref;
      
      public BRModel(ChoicePref<T> pref) {
         this.pref = pref;
      }

      public int getMinimum() { return 0; }
      public int getMaximum() { return pref.validChoices().size() - 1; }
      public int getValue()   { return pref.getSelectedIndex(); }
      public int getExtent()  { return 0; }
      
      public void setValueIsAdjusting(boolean b) {}
      public boolean getValueIsAdjusting() { return false; }
      
      public void setRangeProperties(int value, int extent, int min, int max, boolean adjusting) {
         throw new UnsupportedOperationException();         
      }

      public void addChangeListener(ChangeListener x)    { pref.addChangeListener(x); }
      public void removeChangeListener(ChangeListener x) { pref.removeChangeListener(x); }

      public void setValue(int n)   { if (n >= getMinimum() && n <= getMaximum()) pref.setSelectedIndex(n); }
      public void setExtent(int n)  { throw new UnsupportedOperationException(); }
      public void setMinimum(int n) { throw new UnsupportedOperationException(); }
      public void setMaximum(int n) { throw new UnsupportedOperationException(); }      
   }

   private abstract class CBRenderer extends BasicComboBoxRenderer {
      @Override
      public Component getListCellRendererComponent(JList list, Object value, int index, 
            boolean isSelected, boolean cellHasFocus) {
         return super.getListCellRendererComponent(list, render(value), index, isSelected, cellHasFocus);
      }
      protected abstract Object render(Object value);         
   }
      
   // ======== The Preferences ======================================================================================//
   // ======== Note: you must make sure each preference has a unique key ============================================//

   /** True if Alloy Analyzer should let warning be nonfatal. */
   public static final BooleanPref WarningNonfatal = new BooleanPref("WarningNonfatal", "Allow warnings");

   /** True if Alloy Analyzer should automatically visualize the latest instance. */
   public static final BooleanPref AutoVisualize = new BooleanPref("AutoVisualize", "Visualize automatically");

   /** True if Alloy Analyzer should insist on antialias. */
   public static final BooleanPref AntiAlias = new BooleanPref("AntiAlias", "Use anti-aliasing");

   /** True if Alloy Analyzer should record the raw Kodkod input and output. */
   public static final BooleanPref RecordKodkod = new BooleanPref("RecordKodkod", "Record the Kodkod input/output");

   /** True if Alloy Analyzer should enable the new Implicit This name resolution. */
   public static final BooleanPref ImplicitThis = new BooleanPref("ImplicitThis", 
         "Enable 'implicit this' name resolution");

   /** True if Alloy Analyzer should not report models that overflow. */
   public static final BooleanPref NoOverflow = new BooleanPref("NoOverflow", "Prevent overflows");

   /** The latest X corrdinate of the Alloy Analyzer's main window. */
   public static final IntPref AnalyzerX = new IntPref("AnalyzerX", 0, -1, 65535);

   /** The latest Y corrdinate of the Alloy Analyzer's main window. */
   public static final IntPref AnalyzerY = new IntPref("AnalyzerY", 0, -1, 65535);

   /** The latest width of the Alloy Analyzer's main window. */
   public static final IntPref AnalyzerWidth = new IntPref("AnalyzerWidth", 0, -1, 65535);

   /** The latest height of the Alloy Analyzer's main window. */
   public static final IntPref AnalyzerHeight = new IntPref("AnalyzerHeight", 0, -1, 65535);

   /** The latest font size of the Alloy Analyzer. */
   public static final IntChoicePref FontSize = new IntChoicePref("FontSize", "Font size",
         Arrays.asList(9,10,11,12,14,16,18,20,22,24,26,28,32,36,40,44,48,54,60,66,72), 14);

   /** The latest font name of the Alloy Analyzer. */
   public static final StringChoicePref FontName = new StringChoicePref("FontName", "Font family", 
         Arrays.asList(GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames()), 
         "Lucida Grande");

   /** The latest tab distance of the Alloy Analyzer. */
   public static final IntChoicePref TabSize = IntChoicePref.range("TabSize", "Tab size", 1, 1, 16, 4);

   /** The latest welcome screen that the user has seen. */
   public static final BooleanPref Welcome = new BooleanPref("Welcome", "Show welcome message at start up");

   /** Look and feel */
   public static final StringChoicePref LAF = new StringChoicePref("LAF", "Look and feel", Arrays.asList("Native", "Cross-platform"), 
         Util.onMac() || Util.onWindows() ? "Native" : "Cross-platform");
   
   /** Whether syntax highlighting should be disabled or not. */
   public static final BooleanPref SyntaxDisabled = new BooleanPref("SyntaxHighlightingDisabled", "Disable syntax highlighting");

   /** The number of recursion unrolls. */ 
   public static final IntChoicePref Unrolls = new IntChoicePref("Unrolls", "Recursion depth", Arrays.asList(-1, 0, 1, 2, 3), -1) {
      @Override public Object renderValueShort(Integer value) { return (value != null && value.intValue() == -1) ? "disabled" : value; }
   };
   
   /** The skolem depth. */
   public static final IntChoicePref SkolemDepth = new IntChoicePref("SkolemDepth3", "Skolem depth", Arrays.asList(0, 1, 2, 3, 4), 1);

   /** The unsat core minimization strategy. */
   private static final String[] coreMinimizationLabels = new String[] {
      "Slow", "Slow (guarantees local minimum)", 
      "Medium", "Medium", 
      "Fast", "Fast (initial unsat core)"
   };
   public static final IntChoicePref CoreMinimization = new IntChoicePref("CoreMinimization", "Unsat core minimization", 
         Arrays.asList(0, 1, 2), 2) {
      @Override public Object renderValueShort(Integer value) { return coreMinimizationLabels[value*2]; }
      @Override public Object renderValueLong(Integer value)  { return coreMinimizationLabels[value*2+1]; }
   };
   
   /** The unsat core granularity. */
   private static final String[] coreGranularityLabels = new String[] {
      "Top-level", "Top-level conjuncts only", 
      "Flatten once", "Flatten the formula once at the beginning", 
      "Flatten twice", "Flatten the formula at the beginning and after skolemizing", 
      "Expand quantifiers", "In addition to flattening the formula twice, expand the quantifiers" 
   };
   public static final IntChoicePref CoreGranularity = new IntChoicePref("CoreGranularity", "Unsat core granularity", 
         Arrays.asList(0, 1, 2, 3), 0) {
      @Override public Object renderValueShort(Integer value) { return coreGranularityLabels[value*2]; }
      @Override public Object renderValueLong(Integer value)  { return coreGranularityLabels[value*2+1]; }
   };

   /** The amount of memory (in M) to allocate for Kodkod and the SAT solvers. */
   public static final IntChoicePref SubMemory = new IntChoicePref("SubMemory", "Maximum memory", 
         Arrays.asList(256,512,768,1024,1536,2048,2560,3072,3584,4096), 768) {
      @Override public Object renderValueShort(Integer value) { return value.toString() + " MB"; }
   };
   
   /** The amount of stack (in K) to allocate for Kodkod and the SAT solvers. */
   public static final IntChoicePref SubStack = new IntChoicePref("SubStack", "Maximum stack", 
         Arrays.asList(1024,2048,4096,8192,16384,32768,65536), 8192) {
      @Override public Object renderValueShort(Integer value) { return value.toString() + " k"; }
   };
   
   /** The first file in Alloy Analyzer's "open recent" list. */
   public static final StringPref Model0 = new StringPref("Model0");

   /** The second file in Alloy Analyzer's "open recent" list. */
   public static final StringPref Model1 = new StringPref("Model1");

   /** The third file in Alloy Analyzer's "open recent" list. */
   public static final StringPref Model2 = new StringPref("Model2");

   /** The fourth file in Alloy Analyzer's "open recent" list. */
   public static final StringPref Model3 = new StringPref("Model3");
    
   public static final DelayedChoicePref<SatSolver> Solver = new DelayedChoicePref<SatSolver>("SatSolver2", "Solver", 
         SatSolver.values(), SatSolver.SAT4J) {
      @Override protected String serialize(SatSolver value) { return value.id(); }
   };  
     
   public static final ChoicePref<Verbosity> VerbosityPref = new ChoicePref<Verbosity>("Verbosity", Verbosity.values(), Verbosity.DEFAULT) {
      @Override protected String serialize(Verbosity value) { return value.id; }
   };
   
   // ==============================================================================================//
   
   public enum Verbosity {
      /** Level 0. */  DEFAULT("0", "low"),
      /** Level 1. */  VERBOSE("1", "medium"),
      /** Level 2. */  DEBUG("2", "high"),
      /** Level 3. */  FULLDEBUG("3", "debug");
      /** Returns true if it is greater than or equal to "other". */
      public boolean geq(Verbosity other) { return ordinal() >= other.ordinal(); }
      /** This is a unique String for this value; it should be kept consistent in future versions. */
      private final String id;
      /** This is the label that the toString() method will return. */
      private final String label;
      /** Constructs a new Verbosity value with the given id and label. */
      private Verbosity(String id, String label) { this.id=id; this.label=label; }
      /** Given an id, return the enum value corresponding to it (if there's no match, then return DEFAULT). */
      /** Returns the human-readable label for this enum value. */
      @Override public final String toString() { return label; }
   };
   
   // ==============================================================================================//

   private final Map<Pref<?>, JComponent> pref2comp = new HashMap<Pref<?>, JComponent>();
   private final String binary;
   private final SwingLogPanel log;
   
   public PreferencesDialog(SwingLogPanel log, String binary) {
      this.log = log; 
      this.binary = binary;
      if (log != null && binary != null) {
         Solver.setChoices(testSolvers(), SatSolver.SAT4J);
      } 
      initUI();
   }
   
   protected Iterable<SatSolver> testSolvers() {
      List<SatSolver> satChoices = SatSolver.values().makeCopy();
      satChoices.remove(SatSolver.BerkMinPIPE);
      String fs = System.getProperty("file.separator");
      String test2 = Subprocess.exec(20000, new String[]{binary+fs+"spear", "--model", "--dimacs", binary+fs+"tmp.cnf"});
      if (!isSat(test2)) satChoices.remove(SatSolver.SpearPIPE);
      if (!loadLibrary("minisat")) {
          log.logBold("Warning: JNI-based SAT solver does not work on this platform.\n");
          log.log("This is okay, since you can still use SAT4J as the solver.\n"+
          "For more information, please visit http://alloy.mit.edu/alloy4/\n");
          log.logDivider();
          log.flush();
          satChoices.remove(SatSolver.MiniSatJNI);
      }
      if (!loadLibrary("minisatprover")) satChoices.remove(SatSolver.MiniSatProverJNI);
      if (!loadLibrary("libzchaffmincost")) satChoices.remove(SatSolver.ZChaffJNI);
      SatSolver now = Solver.get();
      if (!satChoices.contains(now)) {
          now=SatSolver.ZChaffJNI;
          if (!satChoices.contains(now)) now=SatSolver.SAT4J;
          Solver.set(now);
      }
      if (now==SatSolver.SAT4J && satChoices.size()>3 && satChoices.contains(SatSolver.CNF) && satChoices.contains(SatSolver.KK)) {
          log.logBold("Warning: Alloy4 defaults to SAT4J since it is pure Java and very reliable.\n");
          log.log("For faster performance, go to Options menu and try another solver like MiniSat.\n");
          log.log("If these native solvers fail on your computer, remember to change back to SAT4J.\n");
          log.logDivider();
          log.flush();
      }
      return satChoices;
   }
   
   /** Returns true iff the output says "s SATISFIABLE" (while ignoring comment lines and value lines) */
   private static boolean isSat(String output) {
       int i=0, n=output.length();
       // skip COMMENT lines and VALUE lines
       while(i<n && (output.charAt(i)=='c' || output.charAt(i)=='v')) {
           while(i<n && (output.charAt(i)!='\r' && output.charAt(i)!='\n')) i++;
           while(i<n && (output.charAt(i)=='\r' || output.charAt(i)=='\n')) i++;
           continue;
       }
       return output.substring(i).startsWith("s SATISFIABLE");
   }
   
   private static boolean loadLibrary(String library) {
      try { System.loadLibrary(library);      return true; } catch(UnsatisfiedLinkError ex) { }
      try { System.loadLibrary(library+"x1"); return true; } catch(UnsatisfiedLinkError ex) { }
      try { System.loadLibrary(library+"x2"); return true; } catch(UnsatisfiedLinkError ex) { }
      try { System.loadLibrary(library+"x3"); return true; } catch(UnsatisfiedLinkError ex) { }
      try { System.loadLibrary(library+"x4"); return true; } catch(UnsatisfiedLinkError ex) { }
      try { System.loadLibrary(library+"x5"); return true; } catch(UnsatisfiedLinkError ex) { return false; }
  }

   protected final void initUI() {
      this.tab = new JTabbedPane();

      tab.addTab("Editor", initEditorPane());
      tab.addTab("Solver", initSolverPane());
      tab.addTab("Miscellaneous", initMiscPane());

      add(tab);
      setTitle("Alloy Preferences");
      pack();
      setSize(getSize().width + 5, getSize().height + 5);
      setResizable(false);
      setLocationRelativeTo(null);
      setAlwaysOnTop(true);
   }

   protected Component initEditorPane() {
      JPanel p = OurUtil.makeGrid(2, gbc().make(), mkCombo(FontName), mkCombo(FontSize), mkCombo(TabSize));
      addToGrid(p, mkCheckBox(SyntaxDisabled), gbc().pos(0, 3).gridwidth(2));
      addToGrid(p, mkCheckBox(AntiAlias),      gbc().pos(0, 4).gridwidth(2));
      
//      JPanel p = new JPanel(new GridBagLayout());
//      addToGrid(p, mkCheckBox(SyntaxDisabled), gbc().pos(0, 0).gridwidth(2));
//      addToGrid(p, mkCheckBox(AntiAlias),      gbc().pos(0, 1).gridwidth(2));
//      addRowToGrid(p, gbc().pos(0, 2), mkComboArr(FontName));
//      addRowToGrid(p, gbc().pos(0, 3), mkComboArr(FontSize));
//      addRowToGrid(p, gbc().pos(0, 4), mkComboArr(TabSize));
      
      return makeTabPane(p);
   }
   
   protected Component initSolverPane() {
      JPanel p = OurUtil.makeGrid(2, gbc().make(), mkCombo(Solver), mkSlider(SkolemDepth), 
            mkCombo(Unrolls), mkCombo(CoreGranularity), mkSlider(CoreMinimization));
      addToGrid(p, mkCheckBox(NoOverflow), gbc().pos(0, 5).gridwidth(2));
      addToGrid(p, mkCheckBox(ImplicitThis), gbc().pos(0, 6).gridwidth(2));
      addToGrid(p, mkCheckBox(RecordKodkod), gbc().pos(0, 7).gridwidth(2));
      
      Solver.addChangeListener(new ChangeListener() {
         public void stateChanged(ChangeEvent e) {
            boolean enableCore = Solver.get() == SatSolver.MiniSatProverJNI;
            pref2comp.get(CoreGranularity).setEnabled(enableCore);
            pref2comp.get(CoreMinimization).setEnabled(enableCore);
         }
      });

      return makeTabPane(p);
   }

   protected Component initMiscPane() {
      JPanel p = OurUtil.makeGrid(2, gbc().make(), mkCombo(SubMemory), mkCombo(SubStack), 
            mkCombo(VerbosityPref), mkCombo(LAF));
      int r = 4;
      addToGrid(p, mkCheckBox(Welcome),          gbc().pos(0, r).gridwidth(2));
      addToGrid(p, mkCheckBox(WarningNonfatal),  gbc().pos(0, r+1).gridwidth(2));
      addToGrid(p, mkCheckBox(AutoVisualize),    gbc().pos(0, r+2).gridwidth(2));
      
//      addToGrid(p, mkCheckBox(Welcome),          gbc().pos(0, 0).gridwidth(2));
//      addToGrid(p, mkCheckBox(WarningNonfatal),  gbc().pos(0, 1).gridwidth(2));
//      addToGrid(p, mkCheckBox(AutoVisualize),    gbc().pos(0, 2).gridwidth(2));      
//      addRowToGrid(p, gbc().pos(0, 3), mkComboArr(SubMemory));
//      addRowToGrid(p, gbc().pos(0, 4), mkComboArr(SubStack));
//      addRowToGrid(p, gbc().pos(0, 5), mkComboArr(VerbosityPref));
      
      return makeTabPane(p);
   }
 
   protected JCheckBox mkCheckBox(final BooleanPref pref) {
      final JCheckBox cb = make(new JCheckBox(pref.getTitleAction()));
      pref2comp.put(pref, cb); 
      ChangeListener ctrl = new ChangeListener() {
         public void stateChanged(ChangeEvent e) {
            cb.setSelected(pref.get());            
         }
      };
      pref.addChangeListener(ctrl);
      ctrl.stateChanged(null);
      return cb;
   }
   
   protected <T> JPanel mkSlider(final ChoicePref<T> pref) {
      final JSlider sl = make(new JSlider(mkBoundedRangeModel(pref)));
      pref2comp.put(pref, sl);
      sl.setMajorTickSpacing(1);
      sl.setMinorTickSpacing(1);
      sl.setPaintTicks(true);
      sl.setPaintLabels(true);
      sl.setSnapToTicks(true);
      sl.setLabelTable(mkDict(pref));
      pref.addChangeListener(new ChangeListener() {
         public void stateChanged(ChangeEvent e) {
            sl.setLabelTable(mkDict(pref));
         }
      });
      sl.addMouseListener(new MouseAdapter() {
         public void mouseReleased(MouseEvent e) {
            SwingUtilities.invokeLater(new Runnable() { public void run() {sl.updateUI();}});
         }
      });
      return OurUtil.makeH(pref.title + ": ", sl);
   }

   private <T> Hashtable<Integer, JLabel> mkDict(final ChoicePref<T> pref) {
      Hashtable<Integer, JLabel> dict = new Hashtable<Integer, JLabel>();
      int sel = pref.getSelectedIndex();
      for (int i = 0; i < pref.validChoices().size(); i++) {
         JLabel label = makeLabel(pref.renderValueShort(pref.validChoices().get(i)));
         if (i == sel) {
            Font font = label.getFont();
            label = OurUtil.make(label, new Font(font.getName(),Font.BOLD,font.getSize()));
         }
         dict.put(i, label);
      }
      return dict;
   }
   
   @SuppressWarnings("unchecked")
   protected <T> JPanel mkCombo(final ChoicePref<T> pref) {
      JComboBox cb = make(new JComboBox(mkComboBoxModelFor(pref)));
      pref2comp.put(pref, cb);
      cb.setRenderer(new CBRenderer() {
         @Override protected Object render(Object value) { return pref.renderValueShort((T)value); }
      });
      return OurUtil.makeH(pref.title + ": ", cb);      
   }
   
   protected <T> Component[] mkComboArr(final ChoicePref<T> pref) {
      return mkCombo(pref).getComponents();
   }
   
   private <T> ComboBoxModel mkComboBoxModelFor(ChoicePref<T> pref)      { return new CBModel<T>(pref); }
   private <T> BoundedRangeModel mkBoundedRangeModel(ChoicePref<T> pref) { return new BRModel<T>(pref); }
   
   private <T extends JComponent> T make(T comp) {
      return OurUtil.make(comp);
   }
   
   private JLabel makeLabel(Object obj) {
      return OurUtil.make(new JLabel(obj.toString()));
   }
   
   private Component makeTabPane(JPanel pane) {
      JPanel p = new JPanel(new GridBagLayout());
      //pane.setBorder(new OurBorder(true, true, true, true));
      p.add(pane, 
            gbc().pos(0, 0)
                 .fill(GridBagConstraints.BOTH)
                 .insets(new Insets(5, 5, 5, 5))
                 .anchor(GridBagConstraints.NORTH)
                 .make());
      p.add(new JLabel(), 
            gbc().pos(0, 1)
                 .weighty(1)
                 .fill(GridBagConstraints.BOTH)
                 .make());
      JPanel ans = OurUtil.make(p, new OurBorder(true, true, true, true));
      return ans;
   }

//   private void addRowToGrid(JPanel p, GridBagConstraintsBuilder builder, Component[] comps) {
//      GridBagConstraints cstr = builder.make();
//      for (int i = 0; i < comps.length; i++) {
//         GridBagConstraints x = (GridBagConstraints) cstr.clone();
//         x.gridx = i;
//         p.add(comps[i], x);
//      }
//   }

   private void addToGrid(JPanel p, Component c, GridBagConstraintsBuilder cstr) {
      p.add(c, cstr.make());
   }

   private GridBagConstraintsBuilder gbc() {
      GridBagConstraintsBuilder ans = new GridBagConstraintsBuilder();
      ans.anchor(GridBagConstraints.WEST)
         .insets(new Insets(3, 3, 3, 3))
         .ipads(3, 3)
         .fill(GridBagConstraints.BOTH);
      return ans;
   }

   public void addChangeListener(ChangeListener l, Pref<?>... prefs) {
      for (Pref<?> pref : prefs) {
         pref.addChangeListener(l);
      }
   }
   
   public static void main(String[] args) {
      SwingUtilities.invokeLater(new Runnable() {
         public void run() {
            PreferencesDialog sd = new PreferencesDialog(null, null);
            sd.setDefaultCloseOperation(EXIT_ON_CLOSE);
            sd.setVisible(true);
         }
      });
   }

}