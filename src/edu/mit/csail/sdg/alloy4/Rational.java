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

package edu.mit.csail.sdg.alloy4;

import java.math.BigInteger;

/**
 * Immutable; represents an infinite-precision rational number.
 */

public final class Rational {

    // Invariant: if b.equals(0), then a==b==BigInteger.ZERO
    // Invariant: if a.equals(0) and !b.equals(0), then a==BigInteger.ZERO and b==BigInteger.ONE

    /** The numerator. */
    private final BigInteger a;

    /** The denominator. */
    private final BigInteger b;

    /** The rational number that results from dividing by 0. */
    public static final Rational NAN = new Rational(BigInteger.ZERO, BigInteger.ZERO);

    /** The rational number representing 1. */
    public static final Rational ZERO = new Rational(BigInteger.ZERO, BigInteger.ONE);

    /** The rational number representing 0. */
    public static final Rational ONE = new Rational(BigInteger.ONE, BigInteger.ONE);

    /** Constructs a new Rational number. */
    private Rational (BigInteger a, BigInteger b) {
        if (b.equals(BigInteger.ZERO)) { this.a=BigInteger.ZERO; this.b=BigInteger.ZERO; }
        else if (a.equals(BigInteger.ZERO)) { this.a=BigInteger.ZERO; this.b=BigInteger.ONE; }
        else { this.a=a; this.b=b; }
    }

    /** Constructs a new Rational number representing the given integer. */
    public static Rational make (int a) {
        if (a==0) return ZERO;
        if (a==1) return ONE;
        return new Rational(new BigInteger(""+a), BigInteger.ONE);
    }

    /** Constructs a new Rational number representing the given numerator divided by the given denomenator. */
    public static Rational make (int a, int b) {
        if (b==0) return NAN;
        if (a==0) return ZERO;
        if (a==b) return ONE;
        if (b<0) return new Rational(new BigInteger(""+a).negate(), new BigInteger(""+b).negate()); else return new Rational(new BigInteger(""+a), new BigInteger(""+b));
    }

    /** Constructs a new Rational number representing the given integer. */
    public static Rational make (BigInteger a) {
        if (a.equals(BigInteger.ZERO)) return ZERO;
        if (a.equals(BigInteger.ONE)) return ONE;
        return new Rational(a, BigInteger.ONE);
    }

    /** Constructs a new Rational number representing the given numerator divided by the given denomenator. */
    public static Rational make (BigInteger a, BigInteger b) {
        if (b.equals(BigInteger.ZERO)) return NAN;
        if (a.equals(BigInteger.ZERO)) return ZERO;
        if (a.equals(b)) return ONE;
        if (b.signum()<0) return new Rational(a.negate(), b.negate()); else return new Rational(a, b);
    }

    /** Returns the result of incrementing this number by 1. */
    public Rational inc () { return add(ONE); }

    /** Returns the result of decrementing this number by 1. */
    public Rational dec () { return sub(ONE); }

    /** Returns the result of negating this number. */
    public Rational neg () { if (b==BigInteger.ZERO) return NAN; else if (a==BigInteger.ZERO) return ZERO; else return make(a.negate(), b); }

    /** Returns the result of adding this number with the given number. */
    public Rational add (Rational r) {
        if (b==BigInteger.ZERO || r.b==BigInteger.ZERO) return NAN;
        if (b.equals(r.b)) return make(a.add(r.a), b);
        return make(a.multiply(r.b).add(b.multiply(r.a)), b.multiply(r.b));
    }

    /** Returns the result of subtracting this number by the given number. */
    public Rational sub (Rational r) {
        if (b==BigInteger.ZERO || r.b==BigInteger.ZERO) return NAN;
        if (b.equals(r.b)) return make(a.subtract(r.a), b);
        return make(a.multiply(r.b).subtract(b.multiply(r.a)), b.multiply(r.b));
    }

    /** Returns the result of multiplying this number by the given number. */
    public Rational mul (Rational r) {
        if (b==BigInteger.ZERO || r.b==BigInteger.ZERO) return NAN;
        return make(a.multiply(r.a), b.multiply(r.b));
    }

    /** Returns the result of dividing this number by the given number. */
    public Rational div (Rational r) {
        if (b==BigInteger.ZERO || r.b==BigInteger.ZERO || r.a==BigInteger.ZERO) return NAN;
        return make(a.multiply(r.b), b.multiply(r.a));
    }

    /** Returns the result of multiplying this number by the given number. */
    public Rational div (int r) {
        if (b==BigInteger.ZERO || r==0) return NAN;
        if (r==1) return this;
        return make(a, b.multiply(new BigInteger(""+r)));
    }

    /** Returns true if this number is undefined (such as that which results from dividing by zero) */
    public boolean isNaN () { return b==BigInteger.ZERO; }

    /** Returns true if this number is not undefined and is negative. */
    public boolean isNeg () { return b!=BigInteger.ZERO && (a.signum() * b.signum() < 0); }

    /** Returns true if this number is not undefined and is zero. */
    public boolean isZero () { return b!=BigInteger.ZERO && a==BigInteger.ZERO; }

    /**
     * Returns -1 if this is less than that, 0 if equal, 1 if this is greater than that;
     * NOTE: two NaN are considered equal, and NaN is considered smaller than all non-NaN.
     */
    public int cmp (Rational that) {
        if (this == that) return 0;
        if (this.b == BigInteger.ZERO) return (that.b==BigInteger.ZERO ? 0 : -1);
        if (that.b == BigInteger.ZERO) return 1;
        Rational diff = sub(that);
        if (diff.isZero()) return 0; else if (diff.isNeg()) return -1; else return 1;
    }

    /** Returns a human-readable representation of this number. */
    @Override public String toString () {
        if (b==BigInteger.ZERO) return "NaN";
        else if (a==BigInteger.ZERO) return "0";
        else if (b.equals(BigInteger.ONE)) return a.toString();
        else return a.toString()+"/"+b;
    }

    /** Returns the value as an approximate floating point number. */
    public strictfp double doubleValue () { return a.doubleValue() / b.doubleValue(); }
}
