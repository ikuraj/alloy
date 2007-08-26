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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4compiler.translator;

import java.util.IdentityHashMap;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.IdentitySet;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.SubsetSig;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SEQIDX;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.NONE;

/**
 * Immutable; this class computes the scopes based on the user's instructions in a "run" or "check" command.
 *
 * <p> In particular, it computes the following 3 information:
 * <br> 1) the bitwidth (always between 1 and 30)
 * <br> 2) the maximum sequence length (always between 0 and 2^(bitwidth-1)-1
 * <br> 3) the scope for each PrimSig, and whether the scope is exact or not
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

    /** The command that we're computing the scope for. */
    private final Command cmd;

    /** The bitwidth. */
    private final int bitwidth;

    /** The maximum sequence length. */
    private final int maxseq;

    /** The scope for each sig. */
    private final IdentityHashMap<PrimSig,Integer> sig2scope = new IdentityHashMap<PrimSig,Integer>();

    /** The sig's scope is exact iff it is in this set. */
    private final IdentitySet<Sig> exact = new IdentitySet<Sig>();

    //===========================================================================================================================//

    /** Returns the integer bitwidth. */
    int getBitwidth() { return bitwidth; }

    /** Returns the maximum allowed sequence length. */
    int getMaxSeq() { return maxseq; }

    /** Returns the scope for a sig (or -1 if we don't know). */
    int sig2scope(Sig sig) { Integer y=sig2scope.get(sig); if (y==null) return -1; else return y; }

    /** Returns whether the scope of a sig is exact or not. */
    boolean isExact(Sig sig) { return (sig instanceof PrimSig) && exact.contains(sig); }

    /** Sets the scope for a sig. */
    private void sig2scope(A4Reporter rep, Sig sig, int newValue) throws Err {
        if (sig.builtin)
           throw new ErrorSyntax(cmd.pos, "Cannot specify a scope for the builtin signature \""+sig+"\"");
        if (sig instanceof SubsetSig)
           throw new ErrorSyntax(cmd.pos, "Cannot specify a scope for a subset signature \""+sig+"\"");
        if (newValue<0)
           throw new ErrorSyntax(cmd.pos, "Cannot specify a negative scope for sig \""+sig+"\"");
        int old=sig2scope(sig);
        if (old==newValue) return;
        if (old>=0)
           throw new ErrorSyntax(cmd.pos, "Sig \""+sig+"\" already has a scope of "+old+", so we cannot set it to be "+newValue);
        sig2scope.put((PrimSig)sig, newValue);
        rep.scope("Sig "+sig+" scope <= "+newValue+"\n");
    }

    /** Make the given sig "exact". */
    private void makeExact(Sig sig) throws Err {
        if (sig instanceof SubsetSig) throw new ErrorSyntax(cmd.pos, "Cannot specify a scope for a subset signature \""+sig+"\"");
        exact.add((PrimSig)sig);
    }

    /**
     * Compute the sum of all "exact" scope of the given sig and its descendents
     * <p> Precondition: sig must not be UNIV
     */
    private int sum(PrimSig sig) throws Err {
        int min=isExact(sig) ? sig2scope(sig) : 0;
        int submin=0;
        for(PrimSig c:sig.children()) {
            int tmp=sum(c);
            if (tmp<0) throw new ErrorSyntax(cmd.pos, "The number of atoms exceeds the internal limit of "+Integer.MAX_VALUE);
            submin=submin+tmp;
            if (submin<0) throw new ErrorSyntax(cmd.pos, "The number of atoms exceeds the internal limit of "+Integer.MAX_VALUE);
        }
        return (min<submin) ? submin : min;
    }

    //===========================================================================================================================//

    /**
     * If A is abstract, unscoped, and all children are scoped, then set A's scope to be the sum;
     * if A is abstract, scoped, and every child except one is scoped, then set that child's scope to be the difference.
     */
    private boolean derive_abstract_scope (A4Reporter rep, Iterable<Sig> sigs) throws Err {
       boolean changed=false;
       again:
       for(Sig s:sigs) if (!s.builtin && (s instanceof PrimSig) && s.isAbstract!=null) {
          SafeList<PrimSig> subs = ((PrimSig)s).children();
          if (subs.size()==0) continue;
          Sig missing=null;
          int sum=0;
          for(Sig c:subs) {
             int cn=sig2scope(c);
             if (cn<0) { if (missing==null) { missing=c; continue; } else { continue again; } }
             sum=sum+cn;
             if (sum<0) throw new ErrorSyntax(cmd.pos, "The number of atoms exceeds the internal limit of "+Integer.MAX_VALUE);
          }
          int sn=sig2scope(s);
          if (sn<0) {
             if (missing!=null) continue;
             sig2scope(rep,s,sum);
             changed=true;
          } else if (missing!=null) {
             sig2scope(rep, missing, (sn<sum) ? 0 : sn-sum);
             changed=true;
          }
       }
       return changed;
    }

    //===========================================================================================================================//

    /**
     * If A is toplevel, and we haven't been able to derive its scope yet, then let it get the "overall" scope.
     */
    private boolean derive_overall_scope (A4Reporter rep, Iterable<Sig> sigs) throws Err {
        boolean changed=false;
        final int overall = (cmd.overall<0 && cmd.scope.size()==0) ? 3 : cmd.overall;
        for(Sig s:sigs) if (!s.builtin && s.isTopLevel() && sig2scope(s)<0) {
            if (overall<0) throw new ErrorSyntax(cmd.pos, "You must specify a scope for sig \""+s+"\"");
            sig2scope(rep, s, overall);
            changed=true;
        }
        return changed;
    }

    //===========================================================================================================================//

    /**
     * If A is not toplevel, and we haven't been able to derive its scope yet, then give it its parent's scope.
     */
    private boolean derive_scope_from_parent (A4Reporter rep, Iterable<Sig> sigs) throws Err {
        boolean changed=false;
        Sig trouble=null;
        for(Sig s:sigs) if (!s.builtin && !s.isTopLevel() && sig2scope(s)<0 && (s instanceof PrimSig)) {
           PrimSig p=((PrimSig)s).parent;
           int pb=sig2scope(p);
           if (pb>=0) {sig2scope(rep,s,pb); changed=true;} else trouble=s;
        }
        if (changed) return true;
        if (trouble==null) return false;
        throw new ErrorSyntax(cmd.pos,"You must specify a scope for sig \""+trouble+"\"");
    }

    //===========================================================================================================================//

    /** Compute the scopes of "world", based on the settings in the "cmd", then log messages to the reporter. */
    ScopeComputer(A4Reporter rep, Iterable<Sig> sigs, Command cmd) throws Err {
        this.cmd=cmd;
        // Process each sig listed in the command
        for(Pair<Sig,Integer> entry:cmd.scope) {
            Sig s=entry.a;
            int scope=entry.b;
            boolean exact=(scope<0);
            if (scope<0) scope=0-(scope+1);
            if (s==UNIV) throw new ErrorSyntax(cmd.pos, "You cannot set a scope on \"univ\".");
            if (s==SIGINT) throw new ErrorSyntax(cmd.pos,
                    "You can no longer set a scope on \"Int\". "
                    +"The number of atoms in Int is always exactly equal to 2^(integer bitwidth).\n");
            if (s==SEQIDX) throw new ErrorSyntax(cmd.pos,
                    "You cannot set a scope on \"seq/Int\". "
                    +"To set the maximum allowed sequence length, use the seq keyword.\n");
            if (s==NONE) throw new ErrorSyntax(cmd.pos, "You cannot set a scope on \"none\".");
            if (s.isOne!=null && scope!=1) throw new ErrorSyntax(cmd.pos,
                "Sig \""+s+"\" has the multiplicity of \"one\", so its scope must be 1, and cannot be "+scope);
            if (s.isLone!=null && scope>1) throw new ErrorSyntax(cmd.pos,
                "Sig \""+s+"\" has the multiplicity of \"lone\", so its scope must 0 or 1, and cannot be "+scope);
            if (s.isSome!=null && scope<1) throw new ErrorSyntax(cmd.pos,
                "Sig \""+s+"\" has the multiplicity of \"some\", so its scope must 1 or above, and cannot be "+scope);
            sig2scope(rep, s, scope);
            if (exact) makeExact(s);
        }
        // Force "one" sigs to be exactly one, and "lone" to be at most one
        for(Sig s:sigs) if (s instanceof PrimSig) {
            if (s.isOne!=null) { makeExact(s); sig2scope(rep,s,1); } else if (s.isLone!=null && sig2scope(s)!=0) sig2scope(rep,s,1);
        }
        // Derive the implicit scopes
        while(true) {
            if (derive_abstract_scope(rep,sigs)) continue;
            if (derive_overall_scope(rep,sigs)) continue;
            if (derive_scope_from_parent(rep,sigs)) continue;
            break;
        }
        // Set the initial scope on "int" and "Int" and "seq"
        int maxseq=cmd.maxseq, bitwidth=cmd.bitwidth;
        if (bitwidth<0) bitwidth=4;
        if (bitwidth<1) throw new ErrorSyntax(cmd.pos, "Cannot specify a bitwidth less than 1");
        if (bitwidth>30) throw new ErrorSyntax(cmd.pos, "Cannot specify a bitwidth greater than 30");
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
        sig2scope.put(SEQIDX, maxseq);
        // Bump up the scope from below
        for(Sig s:sigs) if (s instanceof PrimSig && !s.builtin) {
            PrimSig realS=(PrimSig)s;
            int min=sum(realS), old=sig2scope(s);
            if (old<min) {
                sig2scope.put(realS,min);
                if (isExact(s))
                    rep.scope("Sig "+s+" scope raised from =="+old+" to be =="+min+"\n");
                else
                    rep.scope("Sig "+s+" scope raised from <="+old+" to be <="+min+"\n");
            }
        }
        // Add special overrides for ordered sigs
        for(final Sig s2:sigs) if (s2.isOrdered!=null) {
            if (sig2scope(s2)<=0)
                throw new ErrorSyntax(cmd.pos, "Sig "+s2
                    +" must have a scope of 1 or above, since it is used to instantiate the util/ordering module");
            if (isExact(s2)) continue;
            rep.scope("Sig "+s2+" forced to have exactly "+sig2scope(s2)+" atoms.\n");
            makeExact(s2);
        }
    }

    //===========================================================================================================================//
}
