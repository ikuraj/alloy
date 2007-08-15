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

package edu.mit.csail.sdg.alloy4compiler.parser;

import java.util.Map;
import edu.mit.csail.sdg.alloy4.ConstMap;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;

/**
 * Immutable; reresents a "run" or "check" command.
 *
 * <p> <b>Invariant:</b>  formula is fully typechecked and unambiguous
 * <p> <b>Invariant:</b>  expects == -1, 0, or 1
 * <p> <b>Invariant:</b>  overall >= -1
 * <p> <b>Invariant:</b>  bitwidth >= -1
 * <p> <b>Invariant:</b>  maxseq >= -1
 * <p> <b>Invariant:</b>  none of the signature name is "" or contains '@'
 */

public final class Command {

    /** The position in the original file where this command was declared; never null. */
    public final Pos pos;

    /** The formula associated with this command; always fully-typechecked and unambiguous. */
    public final Expr formula;

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

    /**
     * This maps each signature to a number that represents its bound as well as its exactness.
     * <p> If the number N is >= 0: the sig is bound to have at most N atoms.
     * <p> Otherwise: the sig is bound to have exactly (0-(N+1)) atoms.
     */
    public final ConstMap<String,Integer> scope;

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
        for(Map.Entry<String,Integer> e:scope.entrySet()) {
            sb.append(first?" ":", ");
            int num=e.getValue();
            if (num<0) { sb.append("exactly "); num=0-(num+1); }
            sb.append(num).append(' ').append(e.getKey());
            first=false;
        }
        if (expects>=0) sb.append(" expects ").append(expects);
        return sb.toString();
    }

    /**
     * Constructs a new Command object.
     *
     * @param pos - the original position in the file (must not be null)
     * @param label - the label for this command (it is only for pretty-printing and does not have to be unique)
     * @param formula - the formula associated with this command (it must be fully typechecked and unambiguous)
     * @param check - true if this is a "check"; false if this is a "run"
     * @param overall - the overall scope (0 or higher) (-1 if no overall scope was specified)
     * @param bitwidth - the integer bitwidth (0 or higher) (-1 if it was not specified)
     * @param maxseq - the maximum sequence length (0 or higher) (-1 if it was not specified)
     * @param expects - the expected value (0 or 1) (-1 if no expectation was specified)
     * @param scope - String-to-Integer map (see the "scope" field of the Command class for its meaning)
     *
     * @throws Err if the formula is not already fully-typechecked to be a formula, or is unambiguous
     * @throws Err if at least one of the signature name is "", or contains '@'
     */
    public Command(Pos pos, String label, Expr formula, boolean check, int overall, int bitwidth,
    int maxseq, int expects, Map<String,Integer> scope)
    throws Err {
        if (pos==null) pos = Pos.UNKNOWN;
        formula = formula.typecheck_as_formula();
        if (!formula.errors.isEmpty()) throw formula.errors.get(0);
        if (formula.ambiguous) throw new ErrorType(formula.span(), "This expression is ambiguous.");
        this.pos = pos;
        this.formula = formula;
        this.label = (label==null ? "" : label);
        this.check = check;
        this.overall = (overall<0 ? -1 : overall);
        this.bitwidth = (bitwidth<0 ? -1 : bitwidth);
        this.maxseq = (maxseq<0 ? -1 : maxseq);
        this.expects = (expects<0 ? -1 : (expects>0 ? 1 : 0));
        this.scope = ConstMap.make(scope);
        for(Map.Entry<String,Integer> e:this.scope.entrySet()) {
            if (e.getKey().length()==0) throw new ErrorSyntax(pos, "Signature name cannot be empty.");
            if (e.getKey().indexOf('@')>=0) throw new ErrorSyntax(pos, "Signature name cannot contain \'@\'");
        }
    }

    Command changeFormula(Expr newFormula) throws Err {
        return new Command(pos, label, newFormula, check, overall, bitwidth, maxseq, expects, scope);
    }
}
