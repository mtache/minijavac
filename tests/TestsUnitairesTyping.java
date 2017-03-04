public class A {
    int attr;
    public void m1() {}

    public void m2() {
        m1();
        (new Ext()).mext();
        1 + 1.0;
        55 >> 3;
        true && false;
        return;
    }
}

public class Ext {
    public int mext() {}
}