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

package kodviz.util;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StreamTokenizer;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

/**
 * Generic mechanism for specifying parameters.  Parameters are specified in a parameter
 * file, which contains parameters grouped into parameter groups.  For each parameter,
 * the file specifies its name, type, default value, and description.  Parameter types
 * include boolean, int, double, and enumerated.
 * <p>
 * The parameter file looks like this:
 * <pre>
 * GROUP SYMM
 * PARAM sortComp bool 1 "Sort components"
 * PARAM alignComp bool 1 "Align components"
 * PARAM comparLen int 20 "Comparator length"
 * PARAM addRand bool 0 "Add random symmetries"
 * PARAM nrand int 25 "# of random symmetries to break"
 * ENDGROUP
 * GROUP SATO
 * PARAM useDefines bool 1 "Search only on independent vars"
 * PARAM SplitHeuristic enum First "Splitting heuristic"
 * ENUM SplitHeuristic First "The least unsigned variable in a shortest non-horn clause"
 * ENUM SplitHeuristic Third "Choose the min variable in the first shortest clause"
 * PARAM MaxSaveClauseLen int 20 "Newly discovered clauses <= this length will be added to clause set"
 * ENDGROUP
 * </pre>
 * <p>
 * @author Ian Schechter, Ilya Shlyakhter
 */

@SuppressWarnings("unchecked")
class ParamReader {

    /**
     * Load parameters from the given stream.
     */
    static Params loadParams(InputStream is) throws IOException, ParamFormatException {
        Hashtable solverHash = new Hashtable();
        //InputStreamReader isr = new InputStreamReader(is);
        //BufferedReader br = new BufferedReader(isr);
        Hashtable grp2descr = new Hashtable();
        List grpNameList = new ArrayList();
        StreamTokenizer st = new StreamTokenizer(new InputStreamReader(is));
        st.eolIsSignificant(false);
        st.quoteChar('\"');
        st.parseNumbers();
        st.commentChar('#');
        st.slashStarComments(true);
        st.slashSlashComments(true);
        while (true) {
            int token = st.nextToken();
            if (token == StreamTokenizer.TT_EOF) break;
            check(st.ttype == StreamTokenizer.TT_WORD, "error in param file at line " + st.lineno());
            String lineheader = st.sval;
            if (lineheader.equals("GROUP")) {
                check(st.nextToken()==StreamTokenizer.TT_WORD, "error in param file at line " + st.lineno());
                String grpName = st.sval;
                grpNameList.add(grpName);

                check(st.nextToken()=='\"', "error in param file at line " + st.lineno());
                String description = st.sval;
                grp2descr.put(grpName, description);
                
                _parseParams(grpName, solverHash, st);
            }
        }
        return new Params(solverHash, grp2descr, grpNameList);
    }

    /**
     * Output parameters to the given {@link OutputStream}.
     */
    static void saveParams(OutputStream out_, Params params_) {
        PrintWriter w = new PrintWriter(out_);
        saveParams(w, params_);
    }
    
    /**
     * Output parameters to the given {@link PrintWriter}.
     */
    static void saveParams(PrintWriter w, Params params_) {
        for (Iterator grpNameIter = params_.getGroupNames().iterator(); grpNameIter.hasNext();) {
            String grpName = (String)grpNameIter.next();
            w.println("GROUP " + grpName + " \"" + params_.getGroupDescr(grpName) + "\"");
            for (Iterator paramIter = params_.getGroupParams(grpName).iterator(); paramIter.hasNext();) {
                Parameter param = (Parameter)paramIter.next();
                boolean isString = param.getType()==Parameter.STRING || param.getType()==Parameter.PATH || param.getType()==Parameter.FONT;
                String quote = (isString ? "\"" : "");
                w.println("PARAM " + param.getName() + " " + param.typeString() + " " +
                           quote + param.getValue() + quote +
                          " \"" + param.getMessage() + "\"");
                if (param.getType() == Parameter.ENUM) {
                    Dbg.check(param.getEnum().parameter.equals(param.getName()));
                    for (Iterator enumIter = param.getEnum().values.iterator(); enumIter.hasNext();) {
                        String enumVal = (String)enumIter.next();
                        String enumValDescr = (String)param.getEnum().v2d.get(enumVal);
                        w.println("ENUM " + param.getName() + " " + enumVal + " \"" + enumValDescr + "\"");
                    }
                }
            }
            w.println("ENDGROUP");
        }  // loop over groups
        w.flush();  // do not close: we may be writing to System.out or System.err
    }  // saveParams()
    
    private static void _parseParams(String solver, Hashtable solvers, StreamTokenizer st) throws IOException, ParamFormatException {
        Vector params = new Vector();
        Hashtable enums = new Hashtable();
        while (true) {
            check(st.nextToken() == StreamTokenizer.TT_WORD, "error in param file at line " + st.lineno());
            String next = st.sval;
            if (next.equals("PARAM")) {
                parseParam(st,params);
                solvers.put(solver,params);
            } else
            if (next.equals("ENUM"))
                parseEnum(st,enums);
            else if (next.equals("ENDGROUP")) {
                for (Enumeration e = params.elements(); e.hasMoreElements(); ) {
                    Parameter p = (Parameter) e.nextElement();
                    if (p.getType() == Parameter.ENUM) {
                        EnumParameter en = (EnumParameter) enums.get(p.getName());
                        p.setEnum(en);
                    }
                }
                return;
            } else
                check(false, "error in param file at line " + st.lineno());
        }
    }
    
    private static void parseEnum (StreamTokenizer st, Hashtable h) throws IOException, ParamFormatException {
        check(st.nextToken() == StreamTokenizer.TT_WORD, "error in param file at line " + st.lineno());
        String param = st.sval;
        check(st.nextToken() == StreamTokenizer.TT_WORD, "error in param file at line " + st.lineno());
        String value = st.sval;
        check(st.nextToken() == '\"', "error in param file at line " + st.lineno());
        String description = st.sval;
        
        EnumParameter e = (EnumParameter) h.get(param);
        if (e != null) {
            e.addValueDescription(value,description);
        }
        else {
            e = new EnumParameter(param);
            e.addValueDescription(value,description);
            h.put(param,e);
        }

    }

    private static void parseParam (StreamTokenizer st, Vector params) throws IOException, ParamFormatException {
        check(st.nextToken() == StreamTokenizer.TT_WORD, "error in param file at line " + st.lineno());
        String paramName = st.sval;
        check(st.nextToken() == StreamTokenizer.TT_WORD, "error in param file at line " + st.lineno());
        String paramType = st.sval;
        st.nextToken();
        check((paramType.equals("string") || paramType.equals("path") || 
                 paramType.equals("font")) && st.ttype == '\"'  ||
                paramType.equals("enum") && st.ttype == StreamTokenizer.TT_WORD ||
                (paramType.equals("int") ||
                 paramType.equals("double")) && st.ttype == StreamTokenizer.TT_NUMBER ||
                paramType.equals("bool") && st.ttype == StreamTokenizer.TT_NUMBER &&
                (((int)st.nval) == 0 || ((int)st.nval)==1),
                "error in param file at line " + st.lineno());
        String paramDefault = paramType.equals("string") || paramType.equals("path") || paramType.equals("enum") || 
            paramType.equals("font") ? st.sval : 
            (paramType.equals("double") ? String.valueOf(st.nval) : String.valueOf((int)st.nval));
        
        check(st.nextToken() == '\"', "error in param file at line " + st.lineno());
        String paramMessage = st.sval;

        Parameter p = new Parameter(paramName, paramType, paramDefault, paramMessage);
        params.addElement(p);
    }

    private static void check(boolean b, String msg) throws ParamFormatException {
        if (!b) throw new ParamFormatException(msg);
    }
}
        
