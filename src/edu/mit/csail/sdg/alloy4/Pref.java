package edu.mit.csail.sdg.alloy4;

import java.util.prefs.Preferences;

/**
 * This convenience class reads and writes Java persistent preferences.
 *
 * <p/><b>Thread Safety:</b>  Safe.
 *
 * @author Felix Chang
 */

public class Pref {

    public enum SatSolver {
        BerkMinPIPE("berkmin", "BerkMin"),
        MiniSatSimpPIPE("minisat2simp", "MiniSat2+Simp"),
        MiniSatCorePIPE("minisat2core", "MiniSat2"),
        MiniSatPIPE("minisat", "MiniSat"),
        MiniSatJNI("minisat(jni)", "MiniSat using JNI"),
        ZChaffJNI("zchaff(jni)", "ZChaff using JNI"),
        SAT4J("sat4j", "SAT4J"),
        FILE("file", "Output to file");
        public final String id; // Should be unique, and consistent in future versions
        private final String label;
        private SatSolver(String id, String label) {
            this.id=id; this.label=label;
        }
        public static SatSolver parse(String id) {
            for(SatSolver sc:values()) if (sc.id.equals(id)) return sc;
            return MiniSatPIPE;
        }
        @Override public final String toString() { return label; }
        public void set() { pref.put("SatSolver",id); }
        public static SatSolver get() { return parse(pref.get("SatSolver","")); }
    };

    public enum Verbosity {
        DEFAULT("0", "low"),
        VERBOSE("1", "medium"),
        DEBUG("2", "high");
        public boolean geq(Verbosity other) { return ordinal() >= other.ordinal(); }
        public final String id; // Should be unique, and consistent in future versions
        private final String label;
        private Verbosity(String id, String label) { this.id=id; this.label=label; }
        public static Verbosity parse(String id) {
            for(Verbosity vb:values()) if (vb.id.equals(id)) return vb;
            return DEFAULT;
        }
        @Override public final String toString() { return label; }
        public void set() { pref.put("Verbosity",id); }
        public static Verbosity get() { return parse(pref.get("Verbosity","")); }
    };

    public enum VisualizerMode {
        Viz("graphviz"),
        XML("xml"),
        Tree("tree"),
        KInput("kodkodJava"),
        KOutput("kodkodInstance");
        public final String id; // Should be unique, and consistent in future versions
        private VisualizerMode(String id) { this.id=id; }
        public static VisualizerMode parse(String id) {
            for(VisualizerMode vm:values()) if (vm.id.equals(id)) return vm;
            return Viz;
        }
        public void set() { pref.put("VisualizerMode",id); }
        public static VisualizerMode get() { return parse(pref.get("VisualizerMode","")); }
    };

    public static final BooleanPref ExternalEditor = new BooleanPref("ExternalEditor");
    public static final BooleanPref AutoVisualize = new BooleanPref("AutoVisualize");
    public static final BooleanPref RecordKodkod = new BooleanPref("RecordKodkod");

    public static final IntPref AnalyzerX = new IntPref("AnalyzerX",0,-1,65535);
    public static final IntPref AnalyzerY = new IntPref("AnalyzerY",0,-1,65535);
    public static final IntPref AnalyzerWidth = new IntPref("AnalyzerWidth",0,-1,65535);
    public static final IntPref AnalyzerHeight = new IntPref("AnalyzerHeight",0,-1,65535);
    public static final IntPref FontSize = new IntPref("FontSize",9,12,24);
    public static final IntPref VizX = new IntPref("VizX",0,-1,65535);
    public static final IntPref VizY = new IntPref("VizY",0,-1,65535);
    public static final IntPref VizWidth = new IntPref("VizWidth",0,-1,65535);
    public static final IntPref VizHeight = new IntPref("VizHeight",0,-1,65535);

    public static final StringPref Model0 = new StringPref("Model0");
    public static final StringPref Model1 = new StringPref("Model1");
    public static final StringPref Model2 = new StringPref("Model2");
    public static final StringPref Model3 = new StringPref("Model3");
    public static final StringPref Theme0 = new StringPref("Theme0");
    public static final StringPref Theme1 = new StringPref("Theme1");
    public static final StringPref Theme2 = new StringPref("Theme2");
    public static final StringPref Theme3 = new StringPref("Theme3");

    public static final class BooleanPref {
        private final String id;
        public BooleanPref(String id) { this.id=id; }
        public void set(boolean value) { pref.put(id, value?"y":""); }
        public boolean get() { return pref.get(id,"").length()>0; }
    }

    public static final class IntPref {
        private final String id;
        private final int min,init,max;
        // Requirement: 0 <= min <= max
        // init can be any value, even outside the range; init is returned when the key was not set;
        private int bound(int n) { return n<min ? min : (n>max? max : n); }
        public IntPref(String id, int min, int init, int max) {this.id=id; this.min=min; this.init=init; this.max=max;}
        public void set(int value) { pref.putInt(id,bound(value)); }
        public int get() { if (pref.get(id,"").length()==0) return init; return bound(pref.getInt(id,min)); }
    }

    public static final class StringPref {
        private final String id;
        public StringPref(String id) {this.id=id;}
        public void set(String value) { pref.put(id,value); }
        public String get() { return pref.get(id,""); }
    }

    /** The cached Preferences node. */
    private static final Preferences pref=Preferences.userNodeForPackage(Util.class);
}
