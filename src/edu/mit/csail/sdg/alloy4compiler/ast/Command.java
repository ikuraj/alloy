/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4compiler.ast;

import java.util.List;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;

/**
 * Immutable; reresents a "run" or "check" command.
 *
 * <p> <b>Invariant:</b>  expects == -1, 0, or 1
 * <p> <b>Invariant:</b>  overall >= -1
 * <p> <b>Invariant:</b>  bitwidth >= -1
 * <p> <b>Invariant:</b>  maxseq >= -1
 */

public final class Command {

    /** The position in the original file where this command was declared; never null. */
    public final Pos pos;

    /** The label for the command; it is just for pretty-printing and does not have to be unique. */
    public final String label;

    /** true if this is a "check"; false if this is a "run". */
    public final boolean check;

    /** The overall scope (0 or higher) (Or -1 if there is no overall scope). */
    public final int overall;

    /** The integer bitwidth (0 or higher) (Or -1 if it was not specified). */
    public final int bitwidth;

    /** The maximum sequence length (0 or higher) (Or -1 if it was not specified). */
    public final int maxseq;

    /** The expected answer (either 0 or 1) (Or -1 if there is no expected answer). */
    public final int expects;

    /** The list of scopes. */
    public final ConstList<CommandScope> scope;

    /** This stores a list of Sig whose scope shall be considered "exact", but we may or may not know what its scope is yet. */
    public final ConstList<Sig> additionalExactScopes;

    /** Returns a human-readable string that summarizes this Run or Check command. */
    @Override public final String toString() {
        boolean first=true;
        StringBuilder sb=new StringBuilder(check?"Check ":"Run ").append(label);
        if (overall>=0 && (bitwidth>=0 || maxseq>=0 || scope.size()>0))
            sb.append(" for ").append(overall).append(" but");
        else if (overall>=0)
            sb.append(" for ").append(overall);
        else if (bitwidth>=0 || maxseq>=0 || scope.size()>0)
            sb.append(" for");
        if (bitwidth>=0) { sb.append(" ").append(bitwidth).append(" int"); first=false; }
        if (maxseq>=0) { sb.append(first?" ":", ").append(maxseq).append(" seq"); first=false; }
        for(CommandScope e:scope) {
            sb.append(first?" ":", ").append(e);
            first=false;
        }
        if (expects>=0) sb.append(" expect ").append(expects);
        return sb.toString();
    }

    /** Helper method that converts a (Sig,Integer) pair into a CommandScope object; this is intended for backwards compatibility only. */
    private static CommandScope convert(Pair<Sig,Integer> scope) throws ErrorSyntax {
        boolean exact = false;
        int i = scope.b;
        if (i<0) { exact=true; i=0-(i+1); }
        return new CommandScope(null, scope.a, exact, i, i, 1);
    }

    /** Helper method that converts a list of (Sig,Integer) pairs into a list of CommandScope objects; this is intended for backwards compatibility only. */
    private static ConstList<CommandScope> convert(List<Pair<Sig,Integer>> scope) throws ErrorSyntax {
        if (scope==null) return null;
        TempList<CommandScope> ans = new TempList<CommandScope>(scope.size());
        for(int i=0; i<scope.size(); i++) ans.add(convert(scope.get(i)));
        return ans.makeConst();
    }

    /**
     * Constructs a new Command object.
     *
     * @param pos - the original position in the file (must not be null)
     * @param label - the label for this command (it is only for pretty-printing and does not have to be unique)
     * @param check - true if this is a "check"; false if this is a "run"
     * @param overall - the overall scope (0 or higher) (-1 if no overall scope was specified)
     * @param bitwidth - the integer bitwidth (0 or higher) (-1 if it was not specified)
     * @param maxseq - the maximum sequence length (0 or higher) (-1 if it was not specified)
     * @param expects - the expected value (0 or 1) (-1 if no expectation was specified)
     * @param scope - a list of scopes (can be null if we want to use default)
     * @param additionalExactSig - a list of sigs whose scope shall be considered exact though we may or may not know what the scope is yet
     */
    public Command(Pos pos, String label, boolean check, int overall, int bitwidth,
    int maxseq, int expects, ConstList<CommandScope> scope, Sig... additionalExactSig) {
        if (pos==null) pos = Pos.UNKNOWN;
        this.pos = pos;
        this.label = (label==null ? "" : label);
        this.check = check;
        this.overall = (overall<0 ? -1 : overall);
        this.bitwidth = (bitwidth<0 ? -1 : bitwidth);
        this.maxseq = (maxseq<0 ? -1 : maxseq);
        this.expects = (expects<0 ? -1 : (expects>0 ? 1 : 0));
        this.scope = ConstList.make(scope);
        TempList<Sig> tmp = new TempList<Sig>(additionalExactSig.length);
        for(int i=0; i<additionalExactSig.length; i++) tmp.add(additionalExactSig[i]);
        this.additionalExactScopes = tmp.makeConst();
    }

    /**
     * Constructs a new Command object.
     *
     * @param pos - the original position in the file (must not be null)
     * @param label - the label for this command (it is only for pretty-printing and does not have to be unique)
     * @param check - true if this is a "check"; false if this is a "run"
     * @param overall - the overall scope (0 or higher) (-1 if no overall scope was specified)
     * @param bitwidth - the integer bitwidth (0 or higher) (-1 if it was not specified)
     * @param maxseq - the maximum sequence length (0 or higher) (-1 if it was not specified)
     * @param expects - the expected value (0 or 1) (-1 if no expectation was specified)
     * @param scope - a list that associates each sig with a scope
     */
    public Command(Pos pos, String label, boolean check, int overall, int bitwidth,
    int maxseq, int expects, List<Pair<Sig,Integer>> scope) throws ErrorSyntax {
        this(pos, label, check, overall, bitwidth, maxseq, expects, convert(scope));
    }

    /** Constructs a new Command object where it is the same as the current object, except with a different scope. */
    public Command make(ConstList<CommandScope> scope) {
        return new Command(pos, label, check, overall, bitwidth, maxseq, expects, scope, additionalExactScopes.toArray(new Sig[additionalExactScopes.size()]));
    }

    /** Constructs a new Command object where it is the same as the current object, except with a different list of "additional exact sigs". */
    public Command make(Sig... additionalExactScopes) {
        return new Command(pos, label, check, overall, bitwidth, maxseq, expects, scope, additionalExactScopes);
    }

    /** Constructs a new Command object where it is the same as the current object, except with a different scope for the given sig. */
    public Command make(Sig sig, boolean isExact, int startingScope, int endingScope, int increment) throws ErrorSyntax {
        for(int i=0; i<scope.size(); i++) if (scope.get(i).sig == sig) {
            CommandScope sc = new CommandScope(scope.get(i).pos, sig, isExact, startingScope, endingScope, increment);
            TempList<CommandScope> newlist = new TempList<CommandScope>(scope);
            newlist.set(i, sc);
            return make(newlist.makeConst());
        }
        CommandScope sc = new CommandScope(Pos.UNKNOWN, sig, isExact, startingScope, endingScope, increment);
        return make(Util.append(scope, sc));
    }

    /** Helper method that returns the scope corresponding to a given sig (or return null if the sig isn't named in this command) */
    public CommandScope getScope(Sig sig) {
        for(int i=0; i<scope.size(); i++) if (scope.get(i).sig==sig) return scope.get(i);
        return null;
    }

    /** Helper method that returns true iff this command contains at least one growable sig. */
    public ConstList<Sig> getGrowableSigs() {
        TempList<Sig> answer = new TempList<Sig>();
        for(CommandScope sc: scope) if (sc.startingScope != sc.endingScope) answer.add(sc.sig);
        return answer.makeConst();
    }
}
