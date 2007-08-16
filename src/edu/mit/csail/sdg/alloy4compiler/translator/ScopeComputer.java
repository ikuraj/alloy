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

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4compiler.parser.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.SubsetSig;
import edu.mit.csail.sdg.alloy4compiler.parser.Module;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.NONE;

/**
 * Immutable; this class computes the scopes based on the user's instructions in a "run" or "check" command.
 *
 * <p> The scopes are determined as follows:
 *
 * <p>  "run x": every topsig is scoped to <= 3 elements.
 *
 * <p>  "run x for N": every topsig is scoped to <= N elements.
 *
 * <p>  "run x for N but N1 SIG1, N2 SIG2...":
 * <br> Every sig following "but" is constrained explicitly.
 * <br> Any topsig that is
 * <br> a) not listed, and
 * <br> b) its scope is not derived otherwise
 * <br> will be scoped to have <= N elements.
 *
 * <p>  "run x for N1 SIG1, N2 SIG2..."
 * <br> Every sig following "but" is constrained explicitly.
 * <br> Any topsig that is
 * <br> a) not listed, and
 * <br> b) its scope is not derived otherwise
 * <br> we will give an error message.
 *
 * <p> Please see the code below for the exact rules for deriving the missing scopes.
 */

final class ScopeComputer {

    //==============================================================================================================//
    /** Stores the command that we're computing the scope for. */

    private final Command cmd;

    //==============================================================================================================//
    /** Stores the reporter that we should send the diagnostics messages to. */

    private final A4Reporter rep;

    //==============================================================================================================//
    /** Stores the scope for each sig. */

    private final Map<PrimSig,Integer> sig2scope = new LinkedHashMap<PrimSig,Integer>();

    int sig2scope(Sig sig) { Integer y=sig2scope.get(sig); if (y==null) return -1; return y; }

    private void sig2scope(Sig sig, int newValue) throws Err {
        if (sig.builtin)
            throw new ErrorSyntax(cmd.pos, "You cannot specify a scope for the builtin signature \""+sig+"\"");
        if (sig instanceof SubsetSig)
            throw new ErrorSyntax(cmd.pos, "Can not specify a scope for a subset signature \""+sig+"\"");
        if (newValue<0)
            throw new ErrorSyntax(cmd.pos, "Sig \""+sig+"\" cannot have a negative scope");
        int old=sig2scope(sig);
        if (old==newValue) return;
        if (old>=0)
            throw new ErrorSyntax(cmd.pos, "Sig \""+sig+"\" already has a scope of "
            +old+", so we cannot set it to be "+newValue);
        sig2scope.put((PrimSig)sig,newValue);
        rep.scope("Sig "+sig+" scope <= "+newValue+"\n");
    }

    //==============================================================================================================//
    /** Stores the exactness of each sig's scope. */

    private final Set<PrimSig> exact = new LinkedHashSet<PrimSig>();

    boolean isExact(Sig sig) { if (sig instanceof PrimSig) return exact.contains(sig); else return false; }

    private void makeExact(Sig sig) throws Err {
        if (sig instanceof SubsetSig)
            throw new ErrorSyntax(cmd.pos, "Can not specify a scope for a subset signature \""+sig+"\"");
        exact.add((PrimSig)sig);
    }

    //==============================================================================================================//
    /** Stores the integer bitwidth */

    private final int bitwidth;

    int getBitwidth() { return bitwidth; }

    //==============================================================================================================//
    /** Stores the scope on sequence. */

    private final int maxseq;

    int getMaxSeq() { return maxseq; }

    //==============================================================================================================//

    // If A is abstract, unscoped, and all children are scoped, then we can derive A' scope.
    // If A is abstract, scoped, and every child is scoped except one, then we can derive that child's scope.
    private boolean derive_abstract_scope (Iterable<Sig> sigs) throws Err {
        boolean changed=false;
        again:
        for(Sig s:sigs) if (!s.builtin && (s instanceof PrimSig) && s.isAbstract!=null) {
            SafeList<PrimSig> subs = ((PrimSig)s).children();
            if (subs.size()==0) continue;
            int sn=sig2scope(s);
            if (sn<0) {
                int sum=0;
                for(Sig c:subs) { int cn=sig2scope(c); if (cn>=0) sum=sum+cn; else continue again; }
                sig2scope(s,sum);
                changed=true;
            } else {
                int sum=0;
                Sig cc=null;
                for(Sig c:subs) { int cn=sig2scope(c); if (cn>=0) sum=sum+cn; else if (cc==null) cc=c; else continue again; }
                if (cc!=null) { sig2scope(cc, (sn<sum) ? 0 : sn-sum); changed=true; }
            }
        }
        return changed;
    }

    //==============================================================================================================//

    // If A is toplevel, and we haven't been able to derive its scope yet, then let it get the "overall" scope.
    //
    // After 1 or more execution of this method, every toplevel sig will be scoped
    // (Or else this method would have thrown an exception)
    private boolean derive_overall_scope(Iterable<Sig> sigs) throws Err {
        boolean changed=false;
        final int overall;
        if (cmd.overall<0 && cmd.scope.size()==0) overall=3; else overall=cmd.overall;
        for(Sig s:sigs) if (!s.builtin && s.isTopLevel() && sig2scope(s)<0) {
           if (overall<0) throw new ErrorSyntax(cmd.pos,"No scope specified for top-level type "+s+" in command.");
           sig2scope(s, overall);
           changed=true;
        }
        return changed;
    }

    //==============================================================================================================//

    // If A is not toplevel, and we haven't been able to derive its scope yet, then give it its parent's scope.
    //
    // After 1 execution of this method, every subsig will be scoped.
    // (Or else this method would have thrown an exception)
    private void derive_scope_from_parent(Iterable<Sig> sigs) throws Err {
        while(true) {
            boolean changed=false;
            Sig trouble=null;
            for(Sig s:sigs) if (!s.builtin && !s.isTopLevel() && sig2scope(s)<0 && (s instanceof PrimSig)) {
                PrimSig p=((PrimSig)s).parent;
                int pb=sig2scope(p);
                if (pb>=0) {sig2scope(s,pb); changed=true;} else trouble=s;
            }
            if (!changed) {
                if (trouble==null) return;
                throw new ErrorSyntax(cmd.pos,"No scope specified for subsignature \""+trouble+"\"");
            }
        }
    }

    //==============================================================================================================//

    /** Compute the scopes of "world", based on the settings in the "cmd", then log messages into "options.logger". */
    ScopeComputer(Module root, Command cmd) throws Err {
        this.rep=A4Reporter.getReporter();
        this.cmd=cmd;
        final SafeList<Sig> sigs = root.getAllSigsInTheWorld();
        // Resolve each name listed in the command
        for(Map.Entry<String,Integer> entry:cmd.scope.entrySet()) {
            String name=entry.getKey();
            int scope=entry.getValue();
            boolean exact=(scope<0);
            if (scope<0) scope=0-(scope+1);
            Set<Object> set=root.lookupSigOrParameterOrFunctionOrPredicate(name,false);
            Iterator<Object> it=set.iterator();
            if (set.size()<1) throw new ErrorSyntax(cmd.pos, "The name \""+name+"\" cannot be found");
            if (set.size()>1) {
                Sig choice1=(Sig)(it.next());
                Sig choice2=(Sig)(it.next());
                throw new ErrorSyntax(cmd.pos,
                    "The name \""+name+"\" is ambiguous: it could be "+choice1+" or "+choice2);
            }
            Sig s=(Sig)(it.next());
            if (s==UNIV) throw new ErrorSyntax(cmd.pos, "You cannot set a scope on \"univ\".");
            if (s==SIGINT) throw new ErrorSyntax(cmd.pos,
                    "You can no longer set a scope on \"Int\". "
                    +"The number of atoms in Int is always exactly equal to 2^(integer bitwidth).\n");
            if (s==NONE) throw new ErrorSyntax(cmd.pos, "You cannot set a scope on \"none\".");
            if (exact) makeExact(s);
            if (s.isOne!=null && scope!=1) throw new ErrorSyntax(cmd.pos,
                "Sig \""+s+"\" has the multiplicity of \"one\", so its scope must be 1, and cannot be "+scope);
            if (s.isLone!=null && scope>1) throw new ErrorSyntax(cmd.pos,
                "Sig \""+s+"\" has the multiplicity of \"lone\", so its scope must 0 or 1, and cannot be "+scope);
            if (s.isSome!=null && scope<1) throw new ErrorSyntax(cmd.pos,
                "Sig \""+s+"\" has the multiplicity of \"some\", so its scope must 1 or above, and cannot be "+scope);
            sig2scope(s, scope);
        }
        // Force "one" sigs to be exactly one, and "lone" to be at most one
        for(Sig s:sigs) if (s instanceof PrimSig) {
            if (s.isOne!=null) { makeExact(s); sig2scope(s,1); } else if (s.isLone!=null && sig2scope(s)!=0) sig2scope(s,1);
        }
        // Derive the implicit scopes
        while(true) {
            if (derive_abstract_scope(sigs)) continue;
            if (derive_overall_scope(sigs)) continue;
            derive_scope_from_parent(sigs);
            break;
        }
        // Set the initial scope on "int" and "Int" and "seq"
        int maxseq=cmd.maxseq, bitwidth=cmd.bitwidth;
        if (bitwidth<0) bitwidth=4;
        if (bitwidth<1) throw new ErrorSyntax(cmd.pos, "Cannot specify a bitwidth of 0");
        if (bitwidth>30) throw new ErrorSyntax(cmd.pos, "Cannot specify a bitwidth of greater than 30");
        sig2scope.put(SIGINT, 1<<bitwidth);
        int max=(1<<(bitwidth-1))-1;
        this.bitwidth=bitwidth;
        if (maxseq>max) throw new ErrorSyntax(cmd.pos,
            "With integer bitwidth of "+bitwidth+", you cannot have sequence length longer than "+max);
        if (maxseq<0) {
            if (cmd.overall>=0) maxseq=cmd.overall; else maxseq=4;
            if (maxseq>max) maxseq=max;
        }
        this.maxseq=maxseq;
        // Bump up the scope from below
        for(Sig s:sigs) if (s instanceof PrimSig && !s.builtin) {
            PrimSig realS=(PrimSig)s;
            int min=minimum(realS), old=sig2scope(s);
            if (old<min) {
                sig2scope.put(realS,min);
                rep.scope("Sig "+s+" scope raised from <="+old+" to be <="+min+"\n");
            }
        }
        // Add special overrides for util/ordering
        for(final Sig s:sigs) {
            final Sig s2 = s.getOrderingTarget();
            if (s2!=null) {
                if (sig2scope(s2)<=0)
                    throw new ErrorSyntax(cmd.pos, "Sig "+s2
                            +" must have a scope of 1 or above, since it is used to instantiate the util/ordering.als module");
                if (isExact(s2)) continue;
                rep.scope("Sig "+s2+" forced to have exactly "+sig2scope(s2)+" atoms.\n");
                makeExact(s2);
            }
        }
    }

    //==============================================================================================================//

    /**
     * Compute the sum of all "exact" bounds of the given sig's descendents.
     * <p> Precondition: sig must not be UNIV
     */
    private int minimum(PrimSig sig) throws Err {
        int min=isExact(sig) ? sig2scope(sig) : 0;
        int submin=0;
        for(PrimSig c:sig.children()) submin=submin+minimum(c);
        if (min<submin) min=submin;
        return (min>0) ? min : 0;
    }
}
