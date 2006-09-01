package edu.mit.csail.sdg.alloy4.frontend;

import java.util.ArrayList;
import java.io.FileNotFoundException;

import edu.mit.csail.sdg.alloy4.core.Log;
import edu.mit.csail.sdg.alloy4.core.ParaSig;
import edu.mit.csail.sdg.alloy4.core.Unit;
import edu.mit.csail.sdg.alloy4.core.VisitTypechecker;

public final class Main {

    private static void run(int code, String[] args, Log log) throws FileNotFoundException {
        ArrayList<Unit> units;
        ArrayList<ParaSig> sigs;
        if (args.length==0) {
            System.out.print("% ");
            System.out.flush();
            units=AlloyParser.alloy_totalparseFile("");
            sigs=VisitTypechecker.check(log,units);
            if (code>=(-1)) { VisitEval c=new VisitEval(code,log,units); c.codegen(sigs); }
        }
        else for(String a:args) {
            log.log("\n\nMain file = "+a+"\n");
            units=AlloyParser.alloy_totalparseFile(a);
            sigs=VisitTypechecker.check(log,units);
            if (code>=(-1)) { VisitEval c=new VisitEval(code,log,units); c.codegen(sigs); }
        }
    }

    public static void main(String[] args) throws FileNotFoundException {
        LogToFile log=new LogToFile(".alloy.tmp");
        if (args.length<=1) run(-1,args,log); else run(-2,args,log);
        log.close();
    }
}
