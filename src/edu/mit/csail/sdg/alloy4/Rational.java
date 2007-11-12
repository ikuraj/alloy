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

package edu.mit.csail.sdg.alloy4;

import java.math.BigInteger;

/**
 * Immutable; represents an infinite-precision rational number.
 */

public final class Rational {

    // Invariant: if b.equals(0), then a==b==BigInteger.ZERO
    // Invariant: if a.equals(0) and !b.equals(0), then a==BigInteger.ZERO and b==BigInteger.ONE

    /** The numerator. */
    final BigInteger a;

    /** The denominator. */
    final BigInteger b;

    /** The rational number that results from dividing by 0. */
    public static final Rational NAN = new Rational(BigInteger.ZERO, BigInteger.ZERO);

    /** The rational number representing 1. */
    public static final Rational ZERO = new Rational(BigInteger.ZERO, BigInteger.ONE);

    /** The rational number representing 0. */
    public static final Rational ONE = new Rational(BigInteger.ONE, BigInteger.ONE);

    /** Constructs a new Rational number. */
    private Rational(BigInteger a, BigInteger b) {
        if (b.equals(BigInteger.ZERO)) { this.a=BigInteger.ZERO; this.b=BigInteger.ZERO; }
        else if (a.equals(BigInteger.ZERO)) { this.a=BigInteger.ZERO; this.b=BigInteger.ONE; }
        else { this.a=a; this.b=b; }
    }

    /** Constructs a new Rational number representing the given integer. */
    public static Rational make(int a) {
        if (a==0) return ZERO; else return new Rational(new BigInteger(""+a), BigInteger.ONE);
    }

    /** Constructs a new Rational number representing the given numerator divided by the given denomenator. */
    public static Rational make(int a, int b) {
        return b==0 ? NAN : (a==0 ? ZERO : new Rational(new BigInteger(""+a), new BigInteger(""+b)));
    }

    /** Constructs a new Rational number representing the given integer. */
    public static Rational make(BigInteger a) {
        if (a.equals(BigInteger.ZERO)) return ZERO; else return new Rational(a, BigInteger.ONE);
    }

    /** Constructs a new Rational number representing the given numerator divided by the given denomenator. */
    public static Rational make(BigInteger a, BigInteger b) {
        if (b.equals(BigInteger.ZERO)) return NAN;
        if (a.equals(BigInteger.ZERO)) return ZERO;
        return new Rational(a, b);
    }

    /** Returns the result of incrementing this number by 1. */
    public Rational inc() { return add(ONE); }

    /** Returns the result of decrementing this number by 1. */
    public Rational dec() { return sub(ONE); }

    /** Returns the result of negating this number. */
    public Rational neg() { if (b==BigInteger.ZERO) return NAN; else if (a==BigInteger.ZERO) return ZERO; else return make(a.negate(), b); }

    /** Returns the result of adding this number with the given number. */
    public Rational add(Rational r) {
        if (b==BigInteger.ZERO || r.b==BigInteger.ZERO) return NAN; else if (b.equals(r.b)) return make(a.add(r.a), b); else return make(a.multiply(r.b).add(b.multiply(r.a)), b.multiply(r.b));
    }

    /** Returns the result of subtracting this number by the given number. */
    public Rational sub(Rational r) {
        if (b==BigInteger.ZERO || r.b==BigInteger.ZERO) return NAN; else if (b.equals(r.b)) return make(a.subtract(r.a), b); else return make(a.multiply(r.b).subtract(b.multiply(r.a)), b.multiply(r.b));
    }

    /** Returns the result of multiplying this number by the given number. */
    public Rational mul(Rational r) {
        if (b==BigInteger.ZERO || r.b==BigInteger.ZERO) return NAN; else return make(a.multiply(r.a), b.multiply(r.b));
    }

    /** Returns the result of dividing this number by the given number. */
    public Rational div(Rational r) {
        if (b==BigInteger.ZERO || r.b==BigInteger.ZERO) return NAN; else return make(a.multiply(r.b), b.multiply(r.a));
    }

    /** Returns the result of multiplying this number by the given number. */
    public Rational div(int r) {
        if (b==BigInteger.ZERO || r==0) return NAN; else return make(a, b.multiply(new BigInteger(""+r)));
    }

    /** Returns true if this number is undefined (such as that which results from dividing by zero) */
    public boolean isNaN() { return b==BigInteger.ZERO; }

    /** Returns true if this number is not undefined and is negative. */
    public boolean isNeg() { return b!=BigInteger.ZERO && (a.signum() * b.signum() < 0); }

    /** Returns true if this number is not undefined and is zero. */
    public boolean isZero() { return b!=BigInteger.ZERO && a==BigInteger.ZERO; }

    /**
     * Returns -1 if this is less than that, 0 if equal, 1 if this is greater than that;
     * NOTE: two NaN are considered equal, and NaN is considered smaller than all non-NaN.
     */
    public int cmp(Rational that) {
        if (this==that) return 0;
        if (this.b==BigInteger.ZERO) return (that.b==BigInteger.ZERO ? 0 : -1);
        if (that.b==BigInteger.ZERO) return 1;
        Rational diff = sub(that);
        if (diff.isZero()) return 0; else if (diff.isNeg()) return -1; else return 1;
    }

    /** Returns a human-readable representation of this number. */
    @Override public String toString() {
        if (b==BigInteger.ZERO) return "NaN";
        else if (a==BigInteger.ZERO) return "0";
        else if (b.equals(BigInteger.ONE)) return a.toString();
        else return a.toString()+"/"+b;
    }

    /** Returns the value as an approximate floating point number. */
    public strictfp double doubleValue() { return a.doubleValue() / b.doubleValue(); }
}
