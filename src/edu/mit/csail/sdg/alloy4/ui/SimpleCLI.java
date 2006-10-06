package edu.mit.csail.sdg.alloy4.ui;

import java.util.ArrayList;
import java.util.prefs.Preferences;
import java.io.FileNotFoundException;
import java.io.IOException;
import edu.mit.csail.sdg.alloy4.helper.Log;
import edu.mit.csail.sdg.alloy4.helper.LogToFile;
import edu.mit.csail.sdg.alloy4.node.ParaSig;
import edu.mit.csail.sdg.alloy4.node.Unit;
import edu.mit.csail.sdg.alloy4.node.VisitTypechecker;
import edu.mit.csail.sdg.alloy4.parser.AlloyParser;
import edu.mit.csail.sdg.alloy4.translator.TranslateAlloyToKodkod;

public final class SimpleCLI {

    private static String get(String key) {
        Preferences pref= Preferences.userNodeForPackage(SimpleCLI.class);
        return pref.get(key,"");
    }

    private static final String fs=System.getProperty("file.separator");

    private static void run(int code, String[] args, Log log) throws FileNotFoundException {
        Preferences pref=Preferences.userNodeForPackage(SimpleCLI.class);
        String alloyhome=pref.get("basedir","");
        String dest=alloyhome+fs+"tmp"+fs+"solution.xml";
        ArrayList<Unit> units;
        ArrayList<ParaSig> sigs;
        if (args.length==0) {
            System.out.print("% ");
            System.out.flush();
            units=AlloyParser.alloy_totalparseFile(alloyhome, "");
            sigs=VisitTypechecker.check(log,units);
            if (code>=(-1)) TranslateAlloyToKodkod.codegen(code,log,units,sigs,2,dest);
        }
        else for(String a:args) {
            log.log("\n\nMain file = "+a+"\n");
            units=AlloyParser.alloy_totalparseFile(alloyhome, a);
            sigs=VisitTypechecker.check(log,units);
            if (code>=(-1)) TranslateAlloyToKodkod.codegen(code,log,units,sigs,2,dest);
        }
    }

    public static void main(String[] args) throws IOException {
        String basedir=get("basedir");
        String binary=basedir+fs+"binary";
        try { System.load(binary+fs+"libminisat6.so"); } catch(UnsatisfiedLinkError ex) {
         try { System.load(binary+fs+"libminisat4.so"); } catch(UnsatisfiedLinkError ex2) {
          try { System.load(binary+fs+"libminisat.so"); } catch(UnsatisfiedLinkError ex3) {
           try { System.load(binary+fs+"libminisat.jnilib"); } catch(UnsatisfiedLinkError ex4) {
             System.load(binary+fs+"minisat.dll");
           }
          }
         }
        }
        LogToFile log=new LogToFile(".alloy.tmp");
        if (args.length<=1) run(-1,args,log); else run(-2,args,log);
        log.close();
    }
}
