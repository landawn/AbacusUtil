package com.landawn.abacus.util.function;

public interface ToCharFunction<T> {

    public static final ToCharFunction<Character> UNBOX = new ToCharFunction<Character>() {
        @Override
        public char applyAsChar(Character value) {
            return value == null ? 0 : value.charValue();
        }
    };

    char applyAsChar(T value);
}
