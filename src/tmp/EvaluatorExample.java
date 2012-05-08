package tmp;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.Module;
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Options;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Solution;
import edu.mit.csail.sdg.alloy4compiler.translator.TranslateAlloyToKodkod;

public class EvaluatorExample {

    private static String model =
    		"sig Point {} \n" +
    		"\n" +
    		"run { #Point > 1 } for 3 but 3 Int";
    
    public static void main(String[] args) throws Exception {
        A4Reporter rep = new A4Reporter();
        File tmpAls = File.createTempFile("alloyEvaluator", ".als");
        tmpAls.deleteOnExit();
        flushModelToFile(tmpAls);
        Module world = CompUtil.parseEverything_fromFile(rep, null, tmpAls.getAbsolutePath());
        A4Options opt = new A4Options();
        opt.originalFilename = tmpAls.getAbsolutePath();
        opt.solver = A4Options.SatSolver.SAT4J;
        Command cmd = world.getAllCommands().get(0);
        A4Solution sol = TranslateAlloyToKodkod.execute_commandFromBook(rep, world.getAllReachableSigs(), cmd, opt);
        assert sol.satisfiable();
        
        // eval: univ
        Expr e = CompUtil.parseOneExpression_fromString(world, "univ");        
        System.out.println(sol.eval(e));
        
        // eval: Point
        e = CompUtil.parseOneExpression_fromString(world, "Point");        
        System.out.println(sol.eval(e));
    }

    private static void flushModelToFile(File tmpAls) throws IOException {
        BufferedOutputStream bos = null;
        try {
            bos = new BufferedOutputStream(new FileOutputStream(tmpAls));
            bos.write(model.getBytes());
            bos.flush();
        } finally {
            if (bos != null) bos.close();
        }
    }
    
}
