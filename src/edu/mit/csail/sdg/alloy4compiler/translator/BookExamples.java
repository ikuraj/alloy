/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4compiler.translator;

import static kodkod.engine.Solution.Outcome.SATISFIABLE;
import static kodkod.engine.Solution.Outcome.TRIVIALLY_SATISFIABLE;
import java.util.Set;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.parser.Module;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.config.AbstractReporter;
import kodkod.engine.config.Reporter;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.Tuple;
import kodkod.instance.TupleSet;

class BookExamples {

    /** The reporter that does nothing. */
    private static AbstractReporter blankReporter = new AbstractReporter(){};

    static Solution trial
    (Module world, BoundsComputer bc, Bounds bounds, Formula formula, Solver solver, String originalCommand, String originalFileName) {
        Solution sol=null;
        // int i=originalFileName.lastIndexOf('/');
        // int j=originalFileName.lastIndexOf('\\');
        // String basename = (i>=0 && i>j) ? originalFileName.substring(i+1) :
        //                  ((j>=0 && j>i) ? originalFileName.substring(j+1) : originalFileName);
        if (world.lookupSigOrParameterOrFunctionOrPredicate("this/Book",false).size()>0) {
            Tuple B0N0A0 = t_tuple(bc, "Book[0]", "Name[0]", "Addr[0]");
            Tuple B0N1A0 = t_tuple(bc, "Book[0]", "Name[1]", "Addr[0]");
            Tuple B0N2A0 = t_tuple(bc, "Book[0]", "Name[2]", "Addr[0]");
            Tuple B0N2A1 = t_tuple(bc, "Book[0]", "Name[2]", "Addr[1]");
            Tuple B0N1A1 = t_tuple(bc, "Book[0]", "Name[1]", "Addr[1]");
            Tuple B1N0A0 = t_tuple(bc, "Book[1]", "Name[0]", "Addr[0]");
            Tuple B1N2A1 = t_tuple(bc, "Book[1]", "Name[2]", "Addr[1]");
            Tuple B1N1A1 = t_tuple(bc, "Book[1]", "Name[1]", "Addr[1]");
            Tuple B000 = t_tuple(bc, "Book[0]", "Target[0]", "Target[0]");
            Tuple B001 = t_tuple(bc, "Book[0]", "Target[0]", "Target[1]");
            Tuple B002 = t_tuple(bc, "Book[0]", "Target[0]", "Target[2]");
            Tuple B010 = t_tuple(bc, "Book[0]", "Target[1]", "Target[0]");
            Tuple B101 = t_tuple(bc, "Book[1]", "Target[0]", "Target[1]");
            Tuple B110 = t_tuple(bc, "Book[1]", "Target[1]", "Target[0]");
            Tuple B102 = t_tuple(bc, "Book[1]", "Target[0]", "Target[2]");
            Tuple B210 = t_tuple(bc, "Book[2]", "Target[1]", "Target[0]");
            Tuple B202 = t_tuple(bc, "Book[2]", "Target[0]", "Target[2]");
            Tuple B212 = t_tuple(bc, "Book[2]", "Target[1]", "Target[2]");
            Tuple B302 = t_tuple(bc, "Book[3]", "Target[0]", "Target[2]");
            Tuple B310 = t_tuple(bc, "Book[3]", "Target[1]", "Target[0]");
            Tuple B312 = t_tuple(bc, "Book[3]", "Target[1]", "Target[2]");
            if (sol==null && B000!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 2.9",
                    "Book[0]", "", "this/Book", "",
                    "Target[0]", "", "this/Alias", "",
                    "", "this/Group", "",
                    "", "this/Addr", "",
                    B000, "", "this/Book", "addr",
            });
            if (sol==null && B001!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 2.10",
                    "Book[0]", "", "this/Book", "",
                    "", "this/Alias", "",
                    "Target[0]", "", "this/Group", "",
                    "Target[1]", "Target[2]", "", "this/Addr", "",
                    B001, B002, "", "this/Book", "addr",
            });
            if (sol==null && B001!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 2.11",
                    "Book[0]", "", "this/Book", "",
                    "Target[0]", "", "this/Alias", "",
                    "", "this/Group", "",
                    "Target[1]", "Target[2]", "", "this/Addr", "",
                    B001, B002, "", "this/Book", "addr",
            });
            if (sol==null && B001!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 2.12",
                    "Book[0]", "", "this/Book", "",
                    "Target[0]", "", "this/Alias", "",
                    "Target[1]", "", "this/Group", "",
                    "", "this/Addr", "",
                    B001, "", "this/Book", "addr",
            });
            if (sol==null && B010!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 2.13",
                    "Book[0]", "Book[1]", "", "this/Book", "",
                    "", "this/Alias", "",
                    "Target[0]", "Target[1]", "", "this/Group", "",
                    "Target[2]", "", "this/Addr", "",
                    B010, B110, B102, "", "this/Book", "addr",
            });
            if (sol==null && B312!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 2.15",
                    "Book[0]", "Book[1]", "Book[2]", "Book[3]", "", "this/Book", "",
                    "", "this/Alias", "",
                    "Target[0]", "Target[1]", "", "this/Group", "",
                    "Target[2]", "", "this/Addr", "",
                    B102, B210, B202, B212, B302, B312, "", "this/Book", "addr",
            });
            if (sol==null && B101!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 2.16",
                    "Book[0]", "Book[1]", "Book[2]", "Book[3]", "", "this/Book", "",
                    "Target[1]", "", "this/Alias", "",
                    "Target[0]", "", "this/Group", "",
                    "", "this/Addr", "",
                    B101, "", "this/Book", "addr",
            });
            if (sol==null && B102!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 2.17",
                    "Book[0]", "Book[1]", "Book[2]", "Book[3]", "", "this/Book", "",
                    "Target[0]", "", "this/Alias", "",
                    "Target[1]", "", "this/Group", "",
                    "Target[2]", "", "this/Addr", "",
                    B102, B210, B310, B302, "", "this/Book", "addr",
            });
            if (sol==null && B0N0A0!=null && originalCommand.startsWith("Check "))
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 2.6",
                    "Book[0]", "Book[1]", "", "this/Book", "",
                    "Addr[0]", "", "this/Addr", "",
                    "Name[0]", "", "this/Name", "",
                    B0N0A0, "", "this/Book", "addr",
            });
            if (sol==null && B1N0A0!=null && originalCommand.startsWith("Run "))
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 2.4",
                    "Book[0]", "Book[1]", "", "this/Book", "",
                    "Addr[0]", "", "this/Addr", "",
                    "Name[0]", "", "this/Name", "",
                    B1N0A0, "", "this/Book", "addr",
            });
            if (sol==null && B0N2A1!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 2.5",
                    "Book[0]", "Book[1]", "", "this/Book", "",
                    "Addr[0]", "Addr[1]", "", "this/Addr", "",
                    "Name[0]", "Name[1]", "Name[2]", "", "this/Name", "",
                    B0N2A1, B0N1A1, B1N2A1, B1N1A1, B1N0A0, "", "this/Book", "addr",
            });
            if (sol==null && B0N0A0!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 2.1",
                    "Book[0]", "", "this/Book", "",
                    "Addr[0]", "", "this/Addr", "",
                    "Name[0]", "", "this/Name", "",
                    B0N0A0, "", "this/Book", "addr",
            });
            if (sol==null && B0N0A0!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 2.2",
                    "Book[0]", "", "this/Book", "",
                    "Addr[0]", "", "this/Addr", "",
                    "Name[0]", "Name[1]", "Name[2]", "", "this/Name", "",
                    B0N0A0, B0N1A0, B0N2A0, "", "this/Book", "addr",
            });
            if (sol==null && B0N0A0!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 2.3",
                    "Book[0]", "", "this/Book", "",
                    "Addr[0]", "Addr[1]", "", "this/Addr", "",
                    "Name[0]", "Name[1]", "Name[2]", "", "this/Name", "",
                    B0N0A0, B0N1A0, B0N2A1, "", "this/Book", "addr",
            });
            if (sol==null && B001!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 5.2",
                    "Book[0]", "Book[1]", "", "this/Book", "",
                    "Target[0]", "", "this/Name", "",
                    "Target[1]", "", "this/Addr", "",
                    B001, B101, "", "this/Book", "addr",
            });
            if (sol==null && B102!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 5.3",
                    "Book[0]", "Book[1]", "", "this/Book", "",
                    "Target[0]", "Target[1]", "", "this/Name", "",
                    "Target[2]", "", "this/Addr", "",
                    B010, B110, B102, "", "this/Book", "addr",
            });
        }
        else if (world.lookupSigOrParameterOrFunctionOrPredicate("this/Woman",false).size()>0) {
            Tuple man0_woman0 = t_tuple(bc, "Person[1]", "Person[0]");
            Tuple man1_woman0 = t_tuple(bc, "Person[2]", "Person[0]");
            Tuple man0_woman1 = t_tuple(bc, "Person[1]", "Person[3]");
            Tuple man1_woman1 = t_tuple(bc, "Person[2]", "Person[3]");
            if (sol==null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 4.2",
                    "Person[1]", "", "this/Man", "",
                    "Person[0]", "", "this/Woman", "",
                    man0_woman0, "", "this/Man", "wife",
                    man0_woman0, "", "this/Person", "mother",
                    "", "this/Person", "father",
            });
            if (sol==null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 4.3",
                    "Person[1]", "Person[2]", "", "this/Man", "",
                    "Person[0]", "Person[3]", "", "this/Woman", "",
                    man1_woman0, man0_woman1, "", "this/Man", "wife",
                    man1_woman1, man0_woman0, "", "this/Person", "mother",
                    "", "this/Person", "father",
            });
        }
        else if (world.lookupSigOrParameterOrFunctionOrPredicate("this/Process",false).size()>0) {
            String p0="Process[0]", p1="Process[1]", p2="Process[2]";
            String t0="Time[0]", t1="Time[1]", t2="Time[2]", t3="Time[3]";
            Tuple s20=t_tuple(bc,p2,p0), s01=t_tuple(bc,p0,p1), s12=t_tuple(bc,p1,p2);
            Tuple d000=t_tuple(bc,p0,p0,t0), d110=t_tuple(bc,p1,p1,t0), d220=t_tuple(bc,p2,p2,t0);
            Tuple d001=t_tuple(bc,p0,p0,t1), d021=t_tuple(bc,p0,p2,t1), d111=t_tuple(bc,p1,p1,t1);
            Tuple d002=t_tuple(bc,p0,p0,t2), d112=t_tuple(bc,p1,p1,t2), d122=t_tuple(bc,p1,p2,t2);
            Tuple d003=t_tuple(bc,p0,p0,t3), d113=t_tuple(bc,p1,p1,t3), d223=t_tuple(bc,p2,p2,t3);
            if (sol==null && d000!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 6.4",
                s20, s01, s12, "", "this/Process", "succ",
                d000,d110,d220,d001,d021,d111,d002,d112,d122,d003,d113,d223,"","this/Process","toSend",
                t_tuple(bc,p2,t3),"","this/Process","elected",
            });
        }
        else if (world.lookupSigOrParameterOrFunctionOrPredicate("this/Desk",false).size()>0) {
            String f="Desk[0]", g0="Guest[0]", g1="Guest[1]", r="Room[0]", k0="Key[0]", k1="Key[1]";
            String t0="Time[0]", t1="Time[1]", t2="Time[2]", t3="Time[3]", t4="Time[4]", t5="Time[5]";
            String c0="Card[0]", c1="Card[1]";
            if (sol==null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig E.3",
                t_tuple(bc,c0,k0), t_tuple(bc,c1,k1), "", "this/Card", "fst",
                t_tuple(bc,c0,k1), t_tuple(bc,c1,k0), "", "this/Card", "snd",
                t_tuple(bc,g0,c0,t1),
                t_tuple(bc,g0,c0,t2), t_tuple(bc,g1,c1,t2),
                t_tuple(bc,g0,c0,t3), t_tuple(bc,g1,c1,t3),
                t_tuple(bc,g0,c0,t4), t_tuple(bc,g1,c1,t4),
                t_tuple(bc,g0,c0,t5), t_tuple(bc,g1,c1,t5), "", "this/Guest", "cards",
                t_tuple(bc,r,k0,t0), t_tuple(bc,r,k0,t1), t_tuple(bc,r,k0,t2),
                t_tuple(bc,r,k1,t3), t_tuple(bc,r,k0,t4), t_tuple(bc,r,k1,t5), "", "this/Room", "key",
                t_tuple(bc,f,k1,t1),
                t_tuple(bc,f,k0,t2), t_tuple(bc,f,k1,t2),
                t_tuple(bc,f,k0,t3), t_tuple(bc,f,k1,t3),
                t_tuple(bc,f,k0,t4), t_tuple(bc,f,k1,t4),
                t_tuple(bc,f,k0,t5), t_tuple(bc,f,k1,t5), "", "this/Desk", "issued",
                t_tuple(bc,f,r,k0,t0), t_tuple(bc,f,r,k1,t1), t_tuple(bc,f,r,k0,t2),
                t_tuple(bc,f,r,k0,t3), t_tuple(bc,f,r,k0,t4), t_tuple(bc,f,r,k0,t5), "", "this/Desk", "prev"
            });
        }
        else if (world.lookupSigOrParameterOrFunctionOrPredicate("this/FrontDesk",false).size()>0) {
            String f="FrontDesk[0]", g0="Guest[0]", g1="Guest[1]", r="Room[0]", k0="Key[0]", k1="Key[1]", k2="Key[2]";
            String t0="Time[0]", t1="Time[1]", t2="Time[2]", t3="Time[3]", t4="Time[4]";
            Tuple G0=t_tuple(bc,g0), G1=t_tuple(bc,g1);
            Tuple K0=t_tuple(bc,r,k0), K1=t_tuple(bc,r,k1), K2=t_tuple(bc,r,k2);
            Tuple K0T0=t_tuple(bc,r,k0,t0), K0T1=t_tuple(bc,r,k0,t1), K0T2=t_tuple(bc,r,k0,t2);
            Tuple K0T3=t_tuple(bc,r,k0,t3), K1T4=t_tuple(bc,r,k1,t4);
            Tuple F1=t_tuple(bc,f,r,k0,t0), F2=t_tuple(bc,f,r,k1,t1), F3=t_tuple(bc,f,r,k1,t2);
            Tuple F4=t_tuple(bc,f,r,k2,t3), F5=t_tuple(bc,f,r,k2,t4);
            Tuple GK1=t_tuple(bc,g0,k1,t1), GK2=t_tuple(bc,g0,k1,t2), GK3=t_tuple(bc,g0,k1,t3);
            Tuple GK4=t_tuple(bc,g1,k2,t3), GK5=t_tuple(bc,g0,k1,t4), GK6=t_tuple(bc,g1,k2,t4);
            Tuple O1=t_tuple(bc,f,r,g0,t1), O2=t_tuple(bc,f,r,g1,t3), O3=t_tuple(bc,f,r,g1,t4);
            if (sol==null && K0T0!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 6.13",
                G0, G1, "", "this/Guest", "",
                K0, K1, K2, "", "this/Room", "keys",
                K0T0, K0T1, K0T2, K0T3, K1T4, "", "this/Room", "currentKey",
                F1, F2, F3, F4, F5, "", "this/FrontDesk", "lastKey",
                GK1, GK2, GK3, GK4, GK5, GK6, "", "this/Guest", "keys",
                O1, O2, O3, "", "this/FrontDesk", "occupant",
                "Event[0]", "Event[1]", "", "this/Checkin", "",
                "Event[2]", "", "this/Checkout", "",
                "Event[3]", "", "this/Entry", "",
                t_tuple(bc,"Event[0]",t0),
                t_tuple(bc,"Event[2]",t1),
                t_tuple(bc,"Event[1]",t2),
                t_tuple(bc,"Event[3]",t3), "", "this/Event", "pre",
            });
            if (sol==null && K0T0!=null)
                sol=trial(solver, world, bc, formula, bounds, new Object[]{"Fig 6.6",
                G0, G1, "", "this/Guest", "",
                K0, K1, K2, "", "this/Room", "keys",
                K0T0, K0T1, K0T2, K0T3, K1T4, "", "this/Room", "currentKey",
                F1, F2, F3, F4, F5, "", "this/FrontDesk", "lastKey",
                GK1, GK2, GK3, GK4, GK5, GK6, "", "this/Guest", "keys",
                O1, O2, O3, "", "this/FrontDesk", "occupant",
            });
        }
        return sol;
    }

    private static Solution trial(Solver solver, Module world, BoundsComputer bc, Formula f, final Bounds bb, Object[] t) {
        try {
            // String figure = (String)(t[0]);
            Bounds b = null;
            TupleSet ts = null;
            for(int i=1; i<t.length; i++) {
                Object x=t[i];
                if (x==null) return null;
                if (x instanceof String && ((String)x).length()>0) {
                    Tuple xx = bc.factory().tuple((String)x);
                    if (ts==null) ts=bc.factory().noneOf(xx.arity());
                    ts.add(xx);
                    continue;
                }
                if (x instanceof Tuple) {
                    Tuple xx=(Tuple)x;
                    if (ts==null) ts=bc.factory().noneOf(xx.arity());
                    ts.add(xx);
                    continue;
                }
                if (x instanceof String) {
                    i++;
                    if (i>=t.length-1 || !(t[i] instanceof String) || !(t[i+1] instanceof String)) return null;
                    String sigName = (String)(t[i]);
                    i++;
                    String fieldName = (String)(t[i]);
                    Set<Object> ans = world.lookupSigOrParameterOrFunctionOrPredicate(sigName, false);
                    if (ans.size()!=1) return null;
                    Object first = ans.iterator().next();
                    if (!(first instanceof Sig)) return null;
                    Expression expr = null;
                    if (fieldName.length()==0) {
                        expr = bc.expr((Sig)first);
                    } else {
                        for(Field field: ((Sig)first).getFields()) if (field.label.equals(fieldName)) { expr=bc.expr(field); break; }
                    }
                    if (!(expr instanceof Relation)) return null;
                    if (b==null) b=bb.clone();
                    if (ts==null) ts=bc.factory().noneOf(expr.arity());
                    if (!ts.containsAll(b.lowerBound((Relation)expr))) return null;
                    if (!b.upperBound((Relation)expr).containsAll(ts)) return null;
                    b.boundExactly((Relation)expr, ts);
                    ts=null;
                    continue;
                }
            }
            SATFactory sat = solver.options().solver();
            Reporter reporter = solver.options().reporter();
            Solution sol;
            try {
                solver.options().setSolver(SATFactory.DefaultSAT4J);
                solver.options().setReporter(blankReporter);
                sol = solver.solve(f,b);
            } catch(Throwable ex) {
                sol = null;
            }
            solver.options().setSolver(sat);
            solver.options().setReporter(reporter);
            if (sol==null || (sol.outcome()!=SATISFIABLE && sol.outcome()!=TRIVIALLY_SATISFIABLE)) return null;
            // System.err.println("Match found: "+figure); System.err.flush();
            return sol;
        } catch(Throwable ex) {
            return null;
        }
    }

    private static Tuple t_tuple(BoundsComputer bc, Object... atoms) {
        if (atoms.length <= 0) return null;
        try {
            Tuple t = bc.factory().tuple(atoms);
            return t;
        } catch(Throwable ex) {
            return null;
        }
    }

}
