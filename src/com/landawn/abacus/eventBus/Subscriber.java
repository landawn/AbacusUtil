package com.landawn.abacus.eventBus;

public interface Subscriber<E> {

    public void on(E event);
}
