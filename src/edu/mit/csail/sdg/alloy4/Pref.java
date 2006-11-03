package edu.mit.csail.sdg.alloy4;

import java.util.prefs.Preferences;

/**
 * This class reads and writes Java persistent preferences.
 *
 * <p/><b>Thread Safety:</b>  Safe.
 *
 * @author Felix Chang
 */

public final class Pref {

    /*=======================================================================*/

    /** This enum defines the set of possible SAT solvers. */
    public enum SatSolver {
        /** BerkMin via pipe */               BerkMinPIPE("berkmin", "BerkMin"),
        // MiniSat2(simp) via pipe            MiniSatSimpPIPE("minisat2simp", "MiniSat2+Simp"),
        // MiniSat2(core) via pipe            MiniSatCorePIPE("minisat2core", "MiniSat2"),
        /** MiniSat1 via pipe */              MiniSatPIPE("minisat", "MiniSat"),
        /** MiniSat1 via JNI */               MiniSatJNI("minisat(jni)", "MiniSat using JNI"),
        /** ZChaff via JNI */                 ZChaffJNI("zchaff(jni)", "ZChaff using JNI"),
        /** SAT4J using native Java */        SAT4J("sat4j", "SAT4J"),
        /** Outputs the raw CNF file only */  FILE("file", "Output to file");

        /** This is a unique String for this value; it should be kept consistent in future versions. */
        private final String id;

        /** This is the label that the toString() method will return. */
        private final String label;

        /** Constructs a new SatSolver value with the given id and label. */
        private SatSolver(String id, String label) {
            this.id=id; this.label=label;
        }

        /** Given an id, return the enum value corresponding to it (if there's no match, then return MiniSatPIPE). */
        private static SatSolver parse(String id) {
            for(SatSolver sc:values()) if (sc.id.equals(id)) return sc;
            return MiniSatPIPE;
        }

        /** Returns the human-readable label for this enum value. */
        @Override public final String toString() { return label; }

        /** Saves this value into the Java preference object. */
        public void set() { Preferences.userNodeForPackage(Util.class).put("SatSolver",id); }

        /** Reads the current value of the Java preference object (if it's not set, then return MiniSatPIPE). */
        public static SatSolver get() { return parse(Preferences.userNodeForPackage(Util.class).get("SatSolver","")); }
    };

    /*=======================================================================*/

    /** This enum defines the set of possible message verbosity levels. */
    public enum Verbosity {
        /** Level 0. */  DEFAULT("0", "low"),
        /** Level 1. */  VERBOSE("1", "medium"),
        /** Level 2. */  DEBUG("2", "high");

        /** Returns true if it is greater than or equal to "other". */
        public boolean geq(Verbosity other) { return ordinal() >= other.ordinal(); }

        /** This is a unique String for this value; it should be kept consistent in future versions. */
        private final String id;

        /** This is the label that the toString() method will return. */
        private final String label;

        /** Constructs a new Verbosity value with the given id and label. */
        private Verbosity(String id, String label) { this.id=id; this.label=label; }

        /** Given an id, return the enum value corresponding to it (if there's no match, then return DEFAULT). */
        private static Verbosity parse(String id) {
            for(Verbosity vb:values()) if (vb.id.equals(id)) return vb;
            return DEFAULT;
        }

        /** Returns the human-readable label for this enum value. */
        @Override public final String toString() { return label; }

        /** Saves this value into the Java preference object. */
        public void set() { Preferences.userNodeForPackage(Util.class).put("Verbosity",id); }

        /** Reads the current value of the Java preference object (if it's not set, then return DEFAULT). */
        public static Verbosity get() { return parse(Preferences.userNodeForPackage(Util.class).get("Verbosity","")); }
    };

    /*=======================================================================*/

    /** This enum defines the set of possible visualizer modes. */
    public enum VisualizerMode {
        /** Visualize using graphviz's dot. */    Viz("graphviz"),
        /** See the XML file content. */          XML("xml"),
        /** See the instance as a tree. */        Tree("tree"),
        /** See the raw input to Kodkod. */       KInput("kodkodJava"),
        /** See the raw output from Kodkod. */    KOutput("kodkodInstance");

        /** This is a unique String for this value; it should be kept consistent in future versions. */
        private final String id;

        /** Constructs a new VisualizerMode value with the given id. */
        private VisualizerMode(String id) { this.id=id; }

        /** Given an id, return the enum value corresponding to it (if there's no match, then return Viz). */
        private static VisualizerMode parse(String id) {
            for(VisualizerMode vm:values()) if (vm.id.equals(id)) return vm;
            return Viz;
        }

        /** Saves this value into the Java preference object. */
        public void set() { Preferences.userNodeForPackage(Util.class).put("VisualizerMode",id); }

        /** Reads the current value of the Java preference object (if it's not set, then return Viz). */
        public static VisualizerMode get() {
            return parse(Preferences.userNodeForPackage(Util.class).get("VisualizerMode",""));
        }
    };

    /*=======================================================================*/

    /** True if Alloy Analyzer should use an external text editor rather than the builtin editor. */
    public static final BooleanPref ExternalEditor = new BooleanPref("ExternalEditor");

    /** True if Alloy Analyzer should automatically visualize the latest instance. */
    public static final BooleanPref AutoVisualize = new BooleanPref("AutoVisualize");

    /** True if Alloy Analyzer should record the raw Kodkod input and output. */
    public static final BooleanPref RecordKodkod = new BooleanPref("RecordKodkod");

    /** The latest X corrdinate of the Alloy Analyzer's main window. */
    public static final IntPref AnalyzerX = new IntPref("AnalyzerX",0,-1,65535);

    /** The latest Y corrdinate of the Alloy Analyzer's main window. */
    public static final IntPref AnalyzerY = new IntPref("AnalyzerY",0,-1,65535);

    /** The latest width of the Alloy Analyzer's main window. */
    public static final IntPref AnalyzerWidth = new IntPref("AnalyzerWidth",0,-1,65535);

    /** The latest height of the Alloy Analyzer's main window. */
    public static final IntPref AnalyzerHeight = new IntPref("AnalyzerHeight",0,-1,65535);

    /** The latest font size of the Alloy Analyzer. */
    public static final IntPref FontSize = new IntPref("FontSize",9,12,24);

    /** The latest tab distance of the Alloy Analyzer. */
    public static final IntPref TabSize = new IntPref("TabSize",1,2,16);

    /** The skolem depth. */
    public static final IntPref SkolemDepth = new IntPref("SkolemDepth",0,0,3);

    /** The latest X corrdinate of the Alloy Visualizer window. */
    public static final IntPref VizX = new IntPref("VizX",0,-1,65535);

    /** The latest Y corrdinate of the Alloy Visualizer window. */
    public static final IntPref VizY = new IntPref("VizY",0,-1,65535);

    /** The latest width of the Alloy Visualizer window. */
    public static final IntPref VizWidth = new IntPref("VizWidth",0,-1,65535);

    /** The latest height of the Alloy Visualizer window. */
    public static final IntPref VizHeight = new IntPref("VizHeight",0,-1,65535);

    /** The first file in Alloy Analyzer's "open recent" list. */
    public static final StringPref Model0 = new StringPref("Model0");

    /** The second file in Alloy Analyzer's "open recent" list. */
    public static final StringPref Model1 = new StringPref("Model1");

    /** The third file in Alloy Analyzer's "open recent" list. */
    public static final StringPref Model2 = new StringPref("Model2");

    /** The fourth file in Alloy Analyzer's "open recent" list. */
    public static final StringPref Model3 = new StringPref("Model3");

    /** The first file in Alloy Visualizer's "open recent theme" list. */
    public static final StringPref Theme0 = new StringPref("Theme0");

    /** The second file in Alloy Visualizer's "open recent theme" list. */
    public static final StringPref Theme1 = new StringPref("Theme1");

    /** The third file in Alloy Visualizer's "open recent theme" list. */
    public static final StringPref Theme2 = new StringPref("Theme2");

    /** The fourth file in Alloy Visualizer's "open recent theme" list. */
    public static final StringPref Theme3 = new StringPref("Theme3");

    /*=======================================================================*/

    /**
     * This class reads and writes boolean-valued Java persistent preferences.
     * <p/><b>Thread Safety:</b>  Safe.
     */
    public static final class BooleanPref {
        /** The id associated with this preference. */
        private final String id;
        /** Constructurs a new BooleanPref object with the given id. */
        private BooleanPref(String id) { this.id=id; }
        /** Sets the value for this preference. */
        public void set(boolean value) { Preferences.userNodeForPackage(Util.class).put(id, value?"y":""); }
        /** Reads the value for this preference; if not set, we return false. */
        public boolean get() { return Preferences.userNodeForPackage(Util.class).get(id,"").length()>0; }
    }

    /*=======================================================================*/

    /**
     * This class reads and writes integer-valued Java persistent preferences.
     * <p/><b>Thread Safety:</b>  Safe.
     */
    public static final class IntPref {
        /** The id associated with this preference. */
        private final String id;
        /** The minimum value for this preference. */
        private final int min;
        /** The maximum value for this preference. */
        private final int max;
        /** The default value for this preference. */
        private final int def;
        /** If min>n, we return min; else if n>max, we return max; otherwise we return n. */
        private int bound(int n) { return n<min ? min : (n>max? max : n); }
        /** Constructs a new IntPref object with the given id; you must ensure max >= min */
        private IntPref(String id, int min, int def, int max) {this.id=id; this.min=min; this.def=def; this.max=max;}
        /** Sets the value for this preference. */
        public void set(int value) { Preferences.userNodeForPackage(Util.class).putInt(id,bound(value)); }
        /** Reads the value for this preference; if not set, we return the default value "def". */
        public int get() {
            if (Preferences.userNodeForPackage(Util.class).get(id,"").length()==0) return def;
            return bound(Preferences.userNodeForPackage(Util.class).getInt(id,min));
        }
    }

    /*=======================================================================*/

    /**
     * This class reads and writes String-valued Java persistent preferences.
     * <p/><b>Thread Safety:</b>  Safe.
     */
    public static final class StringPref {
        /** The id associated with this preference. */
        private final String id;
        /** Constructs a new StringPref object with the given id. */
        private StringPref(String id) {this.id=id;}
        /** Sets the value for this preference. */
        public void set(String value) { Preferences.userNodeForPackage(Util.class).put(id,value); }
        /** Reads the value for this preference; if not set, we return the empty string. */
        public String get() { return Preferences.userNodeForPackage(Util.class).get(id,""); }
    }

    /*=======================================================================*/

    /** Constructor is private, since this class never needs to be instantiated. */
    private Pref() { }
}
