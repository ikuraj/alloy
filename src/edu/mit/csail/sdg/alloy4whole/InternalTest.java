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

package edu.mit.csail.sdg.alloy4whole;

import java.util.Arrays;
import kodkod.ast.*;
import kodkod.instance.*;
import kodkod.engine.*;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.config.Options;

/**
 * This class is a convenient place for the developers to test various things with the API,
 * and is not intended nor needed for general use.
 */

final class InternalTest {

    private static final IntConstant n(int i) { return IntConstant.constant(i); }

    private static final int ext(int i) {
        i = i & 15;
        if (i>7) i = i | (-1 ^ 15);
        return i;
    }

    public static void main(String[] args) throws Exception {
        Universe universe = new Universe(Arrays.asList("x"));
        Bounds bounds = new Bounds(universe);
        Solver solver = new Solver();
        solver.options().setSolver(SATFactory.DefaultSAT4J);
        solver.options().setBitwidth(4);
        solver.options().setIntEncoding(Options.IntEncoding.TWOSCOMPLEMENT);
        solver.options().setSymmetryBreaking(20);
        solver.options().setSkolemDepth(0);
        Solution sol = solver.solve(Formula.TRUE, bounds);
        Evaluator ev = new Evaluator(sol.instance());
        ev.options().setBitwidth(4);
        ev.options().setIntEncoding(Options.IntEncoding.TWOSCOMPLEMENT);
        for(int a=-8; a<=7; a++) for(int b=-8; b<=7; b++) {
            String real = "" + ext(a*b);
            String kk = ""+ev.evaluate(n(a).multiply(n(b)));
            System.out.printf("%3d * %3d = %7s %7s %s\n", a, b, real, kk, real.equals(kk) ? "" : "  DIFFERS");
        }
        for(int a=-8; a<=7; a++) for(int b=-8; b<=7; b++) if (b!=0) {
            String real = "" + ext(a/b);
            String kk = ""+ev.evaluate(n(a).divide(n(b)));
            System.out.printf("%3d / %3d = %7s %7s %s\n", a, b, real, kk, real.equals(kk) ? "" : "  DIFFERS");
        }
        System.out.flush();
    }
}

