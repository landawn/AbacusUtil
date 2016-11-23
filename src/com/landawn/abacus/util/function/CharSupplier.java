package com.landawn.abacus.util.function;

public interface CharSupplier {

    public static final CharSupplier ZERO = new CharSupplier() {
        @Override
        public char getAsChar() {
            return 0;
        }
    };

    public static final CharSupplier RANDOM = new CharSupplier() {
        @Override
        public char getAsChar() {
            return (char) Math.abs(Util.RAND.nextInt() % Util.CHAR_MOD);
        }
    };

    char getAsChar();
}