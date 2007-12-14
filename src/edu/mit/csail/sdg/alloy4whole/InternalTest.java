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
        solver.options().setIntEncoding(Options.IntEncoding.BINARY);
        solver.options().setSymmetryBreaking(20);
        solver.options().setSkolemDepth(0);
        Solution sol = solver.solve(Formula.TRUE, bounds);
        Evaluator ev = new Evaluator(sol.instance());
        ev.options().setBitwidth(4);
        ev.options().setIntEncoding(Options.IntEncoding.BINARY);
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

