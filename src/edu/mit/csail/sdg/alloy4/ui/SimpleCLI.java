package edu.mit.csail.sdg.alloy4.ui;

import java.util.ArrayList;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import edu.mit.csail.sdg.alloy4.helper.Log;
import edu.mit.csail.sdg.alloy4.helper.LogToFile;
import edu.mit.csail.sdg.alloy4.node.ParaSig;
import edu.mit.csail.sdg.alloy4.node.Unit;
import edu.mit.csail.sdg.alloy4.node.VisitTypechecker;
import edu.mit.csail.sdg.alloy4.parser.AlloyParser;
import edu.mit.csail.sdg.alloy4.translator.TranslateAlloyToKodkod;
import edu.mit.csail.sdg.alloy4.translator.TranslateAlloyToKodkod.SolverChoice;
import edu.mit.csail.sdg.alloy4.translator.TranslateAlloyToKodkod.Verbosity;
import edu.mit.csail.sdg.alloy4util.Util;

public final class SimpleCLI {

    private static final String fs=System.getProperty("file.separator");

    private static void run(String[] args, Log log) throws FileNotFoundException {
        String destdir = Util.alloyHome()+fs+"tmp";
        File destdirObj = new File(destdir);
        destdirObj.mkdirs();
        ArrayList<Unit> units;
        ArrayList<ParaSig> sigs;
        for(String a:args) {
            log.log("\n\nMain file = "+a+"\n");
            units=AlloyParser.alloy_totalparseFile(Util.alloyHome(), a);
            sigs=VisitTypechecker.check(log,units);
            if (args.length==1) TranslateAlloyToKodkod.codegen(-1, log, Verbosity.DEBUG, units, sigs, SolverChoice.MiniSatJNI, destdir);
        }
    }

    public static void main(String[] args) throws IOException {
        String binary = Util.alloyHome()+fs+"binary"+fs;
        try { System.load(binary+"libminisat6.so"); } catch(UnsatisfiedLinkError ex) {
         try { System.load(binary+"libminisat4.so"); } catch(UnsatisfiedLinkError ex2) {
          try { System.load(binary+"libminisat.so"); } catch(UnsatisfiedLinkError ex3) {
           try { System.load(binary+"libminisat.jnilib"); } catch(UnsatisfiedLinkError ex4) {
             System.load(binary+"minisat.dll");
           }
          }
         }
        }
        LogToFile log=new LogToFile(".alloy.tmp");
        run(args,log);
    }
}
