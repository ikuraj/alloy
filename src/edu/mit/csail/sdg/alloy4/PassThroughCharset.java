package edu.mit.csail.sdg.alloy4;

import java.nio.BufferOverflowException;
import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;

/**
 * This class implements a 8-bit charset that simply passes the stream through.
 * <p> Given byte b, the corresponding character is (char)b.
 * <p> Given char c, the corresponding byte is c if 0..255, and 255 if it's outside of 0..255.
 */
public final class PassThroughCharset extends Charset {

    /** This caches a preconstructed instance of PassThroughCharset. */
    public static final PassThroughCharset instance = new PassThroughCharset("PassThrough", new String[]{});

    /** Constructor is private. */
    private PassThroughCharset(String canonicalName, String[] aliases) { super(canonicalName, aliases); }

    /** @inheritDoc */
    @Override public boolean contains(Charset cs) { return (cs instanceof PassThroughCharset); }

    /** @inheritDoc */
    @Override public CharsetDecoder newDecoder() { return new PassThroughDecoder(); }

    /** @inheritDoc */
    @Override public CharsetEncoder newEncoder() { return new PassThroughEncoder(); }

    private static class PassThroughDecoder extends CharsetDecoder {
        private PassThroughDecoder() {
            super(PassThroughCharset.instance, 1f, 1f);
        }
        @Override protected CoderResult decodeLoop(ByteBuffer in, CharBuffer out) {
            byte b;
            while(true) {
                try {b=in.get();} catch(BufferUnderflowException ex) { return CoderResult.UNDERFLOW; }
                try {out.put((char)b);} catch(BufferOverflowException ex) { return CoderResult.OVERFLOW; }
            }
        }
    }

    private static class PassThroughEncoder extends CharsetEncoder {
        private PassThroughEncoder() {
            super(PassThroughCharset.instance, 1f, 1f);
        }
        @Override protected CoderResult encodeLoop(CharBuffer in, ByteBuffer out) {
            char c;
            byte b;
            while(true) {
                try {c=in.get();} catch(BufferUnderflowException ex) { return CoderResult.UNDERFLOW; }
                if (c>=0 && c<=255) b=(byte)c; else b=(-1);
                try {out.put(b);} catch(BufferOverflowException ex) { return CoderResult.OVERFLOW; }
            }
        }
    }
}
